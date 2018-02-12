{--
=================     ===============     ===============   ================
\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . .//
||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . ..||
|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . .||
||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . ..||
|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . .||
||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|..||
|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| .||
||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / -_.||
||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / | '||
||    `'         || ||         `'    || ||    `'         || ||   | \  / |  ||
||            .===' `===.         .==='.`===.         .===' /==. |  \/  |  ||
||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |  ||
||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |  ||
||   .=='    _-'          `-__\._-'         `-_./__-'         `' |. /|  |  ||
||.=='    _-'                                                     `' | /==.||
=='    _-'                                                            \/  `==
\   _-'                                                                `-_  /
 `''                                                                      ``'
--}

module Wad () where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Data.Word as W
import qualified Data.Array.Unboxed as ARR
import qualified Data.Maybe as M
import Lib

{- |
 - Set of data types and functions for representing and retrieving the header of a DOOM WAD file.
 - The header consists of a 4 byte identity representing wether or not it's an internal WAD or external (IWAD or PWAD)
 - The next 4 bytes are a 32 bit signed integer that keeps count of the number of "lumps" in the WAD
 - The next 4 bytes are a 32 bit signed integer that represents the offset to a lookup table of lumps in the file
 -}

data WadType = IWAD | PWAD
  deriving (Show, Eq)

wadTypeFromStr :: String -> Maybe WadType
wadTypeFromStr "IWAD" = Just IWAD
wadTypeFromStr "PWAD" = Just PWAD
wadTypeFromStr _ = Nothing

getWadType :: G.Get WadType
getWadType = getString 4 >>= \str -> return . M.fromJust . wadTypeFromStr $ str

data WadHeader =
  WadHeader
  {
    wadHeaderType             :: WadType,
    wadHeaderLumpCount        :: I.Int32,
    wadHeaderLumptTableOffset :: I.Int32
  }
  deriving (Show, Eq)

getWadHeader :: G.Get WadHeader
getWadHeader = WadHeader <$> getWadType <*> G.getInt32le <*> G.getInt32le

{-^ End WAD Header -}

---------------------

{- |
 - Set of data types and functions representing and getting lumps and lump tables out of DOOM WAD files.
 - A lump represents some generic piece of game data in DOOM.
 - They contain things such as sound, images, map layouts, etc...
 - Each lump is referenced from the lump table which
 - contains a 4 byte signed integer representing the offset in the file where the lump is located,
 - a 4 byte signed integer representing the lumps size,
 - and an 8 byte string representing the lumps name (names shorter than 8 bytes are padded with '\0')
 -}

data LumpInfo =
  LumpInfo
  {
    lumpOffset :: I.Int32,
    lumpSize   :: I.Int32,
    lumpName   :: String
  }
  deriving (Show, Eq)

getLumpInfo :: G.Get LumpInfo
getLumpInfo = LumpInfo <$> G.getInt32le <*> G.getInt32le <*> (getString lumpNameSize >>=
                                                              \str -> return $! trimPaddedString str)
  where lumpNameSize = 8

type InfoTable = [LumpInfo]

getInfoTable :: Integer -> G.Get InfoTable
getInfoTable n = getList n getLumpInfo

getInfoTableFromHeader :: WadHeader -> G.Get InfoTable
getInfoTableFromHeader (WadHeader _ numLumps offset) = (G.skip . fromIntegral . toInteger $ offset) >>
                                                       (getInfoTable . toInteger $ numLumps)

data Lump =
  Lump
  {
    lumpInfo :: LumpInfo,
    lumpData :: BSL.ByteString
  }
  deriving (Show, Eq)

getLump :: LumpInfo -> G.Get Lump
getLump info = (G.skip . fromIntegral . toInteger . lumpOffset $ info) >>
               (G.getLazyByteString . fromIntegral . toInteger . lumpSize $ info) >>=
               \bytes -> return $! Lump info bytes

getLumps :: InfoTable -> G.Get [Lump]
getLumps = mapM (G.lookAhead . getLump)

isLUMP :: String -> Lump -> Bool
isLUMP s b = s == (lumpName $! lumpInfo b)

{-^ End Lumps -}

---------------

{- |
 - Set of data types and functions for representing and partitioning a DOOM WAD file into its constituent parts.
 - A WAD file can have varying amounts of lumps and lumps with varying names and data.
 - These structures will represent the file with it's header, and a list of generic lumps.
 -}

data WAD = WAD
         {
           wadHeader :: WadHeader,
           wadLumps  :: [Lump]
         }
  deriving (Show, Eq)

getWAD :: G.Get WAD
getWAD = WAD <$> header <*> lumps
  where
    header = G.lookAhead getWadHeader
    infoTable = header >>= \header_ -> G.lookAhead $! getInfoTableFromHeader header_
    lumps = infoTable >>= \infoTable_ -> getLumps infoTable_

{-^ End WAD -}

--------------

{- |
 - COLORMAP is a lump containing brightness and color shift information.
 - Each index maps a specific byte to a specific palette in the PLAYPAL.
 - Each color map is 256 byte long table.
 - There are a total of 34 color maps indexed from 0 - 33
 - 0 - 31 the first 32 maps index brightness in decreasing order; where 0 is max brightness and 31 is complete darkness.
 - 32 is an invunreablity powerup and 33 is all black.
 -}

type PaletteIndex = W.Word8
type MapIndex = W.Word8
type Map = ARR.Array MapIndex PaletteIndex

getMap :: G.Get Map
getMap = getList 256 G.getWord8 >>= \x -> return $! ARR.listArray (0,255) x

type ColorMapIndex = W.Word8
type COLORMAP = ARR.Array ColorMapIndex Map

getCOLORMAP :: G.Get COLORMAP
getCOLORMAP = getList 34 getMap >>= \x -> return $! ARR.listArray (0,33) x

colorMapByte :: Map -> MapIndex -> PaletteIndex
colorMapByte cmap byte = cmap ARR.! (fromIntegral $! toInteger byte)

selectMap :: COLORMAP -> ColorMapIndex -> (MapIndex -> PaletteIndex)
selectMap cmap byte = colorMapByte (cmap ARR.! (fromIntegral $! toInteger byte))

isCOLORMAP :: Lump -> Bool
isCOLORMAP = isLUMP "COLORMAP"

{-^ End COLORMAP -}

-------------------

{- |
 - PLAYPAL represents the colors used by DOOM in representing pixel colors.
 - The PLAYPAL contains 14 palettes with 256 3 byte RGB triples in each palette.
 -}

type ColorChannel = W.Word8

data RGB =
  RGB
  {
    redChannel   :: ColorChannel,
    greenChannel :: ColorChannel,
    blueChannel  :: ColorChannel
  }

getRGB :: G.Get RGB
getRGB = RGB <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

type Palette = ARR.Array PaletteIndex RGB

getPalette :: G.Get Palette
getPalette = getList 256 getRGB >>= \palette -> return $! ARR.listArray (0,255) palette

type PlaypalIndex = W.Word8
type PLAYPAL = ARR.Array Integer Palette

getPLAYPAL :: G.Get PLAYPAL
getPLAYPAL = getList 14 getPalette >>= \playpal -> return $! ARR.listArray (0,14) playpal

selectRGB :: Palette -> PaletteIndex -> RGB
selectRGB palette byte = palette ARR.! (fromIntegral $! toInteger byte)

selectPalette :: PLAYPAL -> PlaypalIndex -> (PaletteIndex -> RGB)
selectPalette ppal byte = selectRGB (ppal ARR.! (fromIntegral $! toInteger byte))

isPLAYPAL :: Lump -> Bool
isPLAYPAL = isLUMP "PLAYPAL"

{-^ End PLAYPAL -}

------------------

{- | Picture format -}

data PictureHeader =
  PictureHeader
  {
    pictureWidth  :: I.Int16, -- Number of columns of picture data
    pictureHeight :: I.Int16, -- Number of rows
    picXOffset    :: I.Int16, -- Number of pixels to the left of the center where the first column gets drawn.
    picYOffset    :: I.Int16  -- Number of pixels above the origin where the top row is.
  }

getPictureHeader :: G.Get PictureHeader
getPictureHeader = PictureHeader <$> G.getInt16le <*> G.getInt16le <*> G.getInt16le <*> G.getInt16le

data Column =
  Column
  {
    columnDownwardsOffset   :: W.Word8,
    columnColoredPixelCount :: W.Word8,
    columnPixels            :: [W.Word8]
  }

getColumn :: G.Get Column
getColumn = G.getWord8 >>=
            \offset -> G.getWord8 >>=
            \count  -> G.skip 1 >>=
            \_      -> getList (fromIntegral . toInteger $ count) G.getWord8 >>=
            \pixels -> G.skip 1 >>=
            \_      -> return $! Column offset count pixels

type Post = [Column]


--NOTE This is kinda dirty and not haskellish clean it up if you can.
getPost :: G.Get Post
getPost = getDirtyPost >>= \columns -> return $! filter (\column -> columnDownwardsOffset column /= 255) columns
  where
    getDirtyPost               = G.lookAhead G.getWord8 >>= \offset -> getColumnBasedOnOffset . toInteger $ offset
    getColumnBasedOnOffset 255 = getColumn >>= \column -> return [column]
    getColumnBasedOnOffset _   = getColumn >>= \column -> getPost >>= \post -> return $! [column] ++ post

getPostAtOffset :: Integer -> G.Get Post
getPostAtOffset n = G.skip (fromIntegral n) >>= \_ -> getPost

data RawPicture =
  RawPicture
  {
    rawPicHeader  :: PictureHeader,
    rawPicGolumns :: [Post]
  }

getRawPicture :: G.Get RawPicture
getRawPicture = G.lookAhead getPictureHeader >>=
                \header  -> G.lookAhead (getPictureHeader >>= \_ -> getList (fromIntegral .toInteger . pictureWidth $ header) G.getInt32le) >>=
                \offsets -> mapM (G.lookAhead . getPostAtOffset . fromIntegral . toInteger) offsets >>=
                \columns -> return $! RawPicture header columns

type PictureColorIndexes = [[Integer]]

--postToIndexes :: PictureHeader -> Post -> [Integer]
--postToIndexes ph p = 

--rawPicToIndexedPic :: RawPicture -> PictureColorIndexes

type Picture = [[RGB]]

data AnimationFrames = AniA | AniB | AniC | AniD | AniE | AniF | AniG | AniH | AniI | AniJ |
                       AniK | AniL | AniM | AniN | AniO | AniP | AniQ | AniR | AniS | AniT |
                       AniU | AniV | AniW | AniX | AniY | AniZ

{-^ End Picture Format-}

------------------------

{- |
 - ENDOOM is a lump containing the endscreen for when you exit the game.
 - It is just 4000 bytes representing an 80 by 25 ansi screen.
 -}

type AnsiColor = W.Word8

data AnsiCharacter =
  AnsiCharacter
  {
    ansiChar  :: Char,
    ansiColor :: AnsiColor
  }

getAnsiCharacter :: G.Get AnsiCharacter
getAnsiCharacter = AnsiCharacter <$> getChar_ <*> G.getWord8

type ENDDOOM = ARR.Array Integer AnsiCharacter

getENDOOM :: G.Get ENDDOOM
getENDOOM = getList 2000 getAnsiCharacter >>= \x -> return $! ARR.listArray (0, 1999) x

{-^ End ENDOOM -}

-----------------

{- |
 - DEMON -}

{-^ End DEMON -}

{- |
 - TEXTUREN
 - There are two types of TEXTURE lumps one in Doom1 wad (shareware), and one in Doom wad (paid)
 - They each contain the set of wall textures used in the game.
 -}

data Patch =
  Patch
  {
    patchXOffset   :: I.Int16,
    patchYOffset   :: I.Int16,
    patchNameIndex :: I.Int16
  }

getPatch :: G.Get Patch
getPatch = Patch <$> G.getInt16le <*> G.getInt16le <*> G.getInt16le

getPatches :: Integer -> G.Get [Patch]
getPatches n = getList n getPatch

data Texture =
  Texture
  {
    textureName    :: String,
    textureWidth   :: I.Int16,
    textureHeight  :: I.Int16,
    texturePatches :: [Patch]
  }

getTexture :: G.Get Texture
getTexture = getString 8 >>=
              \name       -> G.skip 4 >>=
              \_          -> G.getInt16le >>=
              \width      -> G.getInt16le >>=
              \height     -> G.skip 4 >>=
              \_          -> G.getInt16le >>=
              \numPatches -> (getPatches $! toInteger numPatches) >>=
              \patches    -> return $! Texture name width height patches

type TEXTURE = [Texture]

getTEXTURE :: G.Get TEXTURE
getTEXTURE = G.lookAhead G.getInt32le >>=
             \nums    -> G.lookAhead $! getList  (fromIntegral $! toInteger nums) G.getInt32le >>=
             \ptrs    -> (return $! map (fromIntegral . toInteger) ptrs) >>=
             \offsets -> mapM (\x -> G.lookAhead (G.skip x >>= (\_ -> getTexture))) offsets

isTEXTURE :: Lump -> Bool
isTEXTURE l = or [(isLUMP "TEXTURE1" l), (isLUMP "TEXTURE2" l)]

{-^ End TEXTUREN -}

-------------------

{- | SPRITES -}

data Sprite = PLAY | POSS | SSWV | SPOS | CPOS | TROO | SARG | SKUL | HEAD | BOS2 |
              BOSS | BSPI | PAIN | SKEL | FATT | VILE | SPID | CYBR | BBRN | CSAW |
              SHOT | SGN2 | MGUN | LAUN | PLAS | BFUG | CLIP | SHEL | ROCK | CELL |
              AMMO | SBOX | BROK | CELP | BPAK | STIM | MEDI | BON1 | BON2 | ARM1 |
              ARM2 | MEGA | SOUL | PINV | PSTR | PINS | SUIT | PMAP | PVIS | BKEY |
              BSKU | RKEY | RSKU | YKEY | YSKU | BAR1 | KEEN | ELEC | COL1 | COL3 |
              COL2 | COL5 | COL4 | COL6 | SMIT | TRE1 | TRE2 | COLU | TLMP | TLP2 |
              CAND | CBRA | TBLU | TGRE | TRED | SMBT | SMGT | SMRT | FCAN | CEYE |
              FSKU | POL1 | GOR1 | GOR2 | GOR3 | GOR4 | GOR5 | HDB1 | HDB2 | HDB3 |
              HDB4 | HDB5 | HDB6 | POL6 | POL4 | POL2 | POL3 | POL5 | POB1 | POB2 |
              BRS1

{-^ End SPRITES-}


{- |
 - PNAMES
 - PNAMES represent names used by walls in doom
 -}

type PNAME = String

getPNAME :: G.Get PNAME
getPNAME = getString 8

type PNAMES = [String]

getPNAMES :: G.Get PNAMES
getPNAMES = G.getInt32le >>=
            \numNames -> getList (fromIntegral . toInteger $ numNames) getPNAME >>=
            \names    -> return names

{-^ End PNAMES-}

----------------



{- |
 - THINGS represent players, monsters, pick-ups, and projectiles.
 - Internally these things are called actors.
 - -}

data ThingTypeInfo =
  ThingTypeInfo
  {
    thingTypeInfoIndex   :: Integer,
    thingTypeInfoDoomVer :: Integer
  }

-- | Data type representing a "thing" in DOOM.
data ThingType =
  ZeroThing                        |
  Player1Start                     |
  Player2Start                     |
  Player3Start                     |
  Player4Start                     |
  DeathMatchStart                  |
  FormerHuman                      |
  WolfensteinOfficer               |
  FormerHumanSeargent              |
  FormerHumanComando               |
  Imp                              |
  Demon                            |
  Spectre                          |
  LostSoul                         |
  Cacodemon                        |
  HellKnight                       |
  BaronOfHell                      |
  Arachnotron                      |
  PainElemental                    |
  Revenant                         |
  Mancubus                         |
  Archvile                         |
  SpiderDemon                      |
  CyberDemon                       |
  BossBrain                        |
  TeleportLanding                  |
  BossShooter                      |
  SpawnSpot                        |
  Chainsaw                         |
  Shotgun                          |
  DoubleBarreledShotgun            |
  Chaingun                         |
  RocketLauncher                   |
  Plasmagun                        |
  BFG9000                          |
  AmmoClip                         |
  ShotgunShells                    |
  Rocket                           |
  CellCharge                       |
  BoxOfAmmo                        |
  BoxOfShells                      |
  BoxOfRockets                     |
  CellChargePack                   |
  Backpack                         |
  Stimpack                         |
  Medikit                          |
  HealthPotion                     |
  SpiritArmor                      |
  SecurityArmor                    |
  CombatArmor                      |
  MegaSphere                       |
  SoulSphere                       |
  Invulnerability                  |
  BeserkPack                       |
  Invisibility                     |
  RadiationSuit                    |
  ComputerMap                      |
  LightAmplificationGoggles        |
  BlueKeyCard                      |
  RedKeyCard                       |
  YellowKeyCard                    |
  BlueSkullKey                     |
  RedSkullKey                      |
  YellowSkullKey                   |
  Barrel                           |
  BurningBarrel                    |
  Candle                           |
  Candleabra                       |
  TallTechnoColumn                 |
  TallGreenPillar                  |
  TallRedPillar                    |
  ShortGreenPillar                 |
  ShortGreenPillarWithHeart        |
  ShortGreenPillarWithBeatingHeart |
  ShortRedPillar                   |
  ShortRedPillarWithSkull          |
  Stalagmite                       |
  BurntGreyTree                    |
  LargeBrownTree                   |
  TallBlueFireStick                |
  TallGreenFireStick               |
  TallRedFireStick                 |
  ShortBlueFireStick               |
  ShortGreenFireStick              |
  ShortRedFireStick                |
  FloorLamp                        |
  TallTechnoLamp                   |
  ShortTechnoLamp                  |
  EvilEyeSymbol                    |
  FlamingSkullRock                 |
  ImpaledHuman                     |
  TwitchingImpaledHuman            |
  SkullOnPole                      |
  FiveSkullShishKebab              |
  PileOfSkullsAndCandles           |
  HangingVictim                    |
  HangingVictimTwitching           |
  HangingPairOfLegs                |
  HangingVictim1Legged             |
  HangingLeg                       |
  HangingVictimNoGuts              |
  HangingVictimNoGutsBrain         |
  HangingTorsoLookingDown          |
  HangingTorsoOpenSkull            |
  HangingTorsoLookingUp            |
  HangingTorsoNoBrain              |
  HangingBilly                     |
  DeadPlayer                       |
  DeadFormerHuman                  |
  DeadFormerSeargent               |
  DeadImp                          |
  DeadDemon                        |
  DeadCacodemon                    |
  DeadLostSoulInvisible            |
  BloodyMessExplodedPlayer         |
  BloodyMessAbove                  |
  PoolOfBlood                      |
  PoolOfGuts                       |
  SmallPoolOfGuts                  |
  PoolOfBrains                     |
  HangingVictimTwitching2          |
  HangingVictimArmsSpread          |
  HangingVictim1Legged2            |
  HangingPairOfLegs2               |
  HangingLeg2                      |
  UnknownThing Integer

thingTypeFromInteger :: Integer -> ThingType
thingTypeFromInteger (-1) = ZeroThing
thingTypeFromInteger 0    = ZeroThing
thingTypeFromInteger 1    = Player1Start
thingTypeFromInteger 2    = Player2Start
thingTypeFromInteger 3    = Player3Start
thingTypeFromInteger 4    = Player4Start
thingTypeFromInteger 11   = DeathMatchStart
thingTypeFromInteger 14   = TeleportLanding
thingTypeFromInteger 3004 = FormerHuman
thingTypeFromInteger 84   = WolfensteinOfficer
thingTypeFromInteger 9    = FormerHumanSeargent
thingTypeFromInteger 65   = FormerHumanComando -- Also known as heavy weapon dude in some docs (No joke)
thingTypeFromInteger 3001 = Imp
thingTypeFromInteger 3002 = Demon
thingTypeFromInteger 58   = Spectre
thingTypeFromInteger 3006 = LostSoul
thingTypeFromInteger 3005 = Cacodemon
thingTypeFromInteger 69   = HellKnight
thingTypeFromInteger 3003 = BaronOfHell
thingTypeFromInteger 68   = Arachnotron
thingTypeFromInteger 71   = PainElemental
thingTypeFromInteger 66   = Revenant
thingTypeFromInteger 67   = Mancubus
thingTypeFromInteger 64   = Archvile
thingTypeFromInteger 7    = SpiderDemon -- Also known as spider mastermind in some docs
thingTypeFromInteger 16   = CyberDemon
thingTypeFromInteger 88   = BossBrain
thingTypeFromInteger 89   = BossShooter
thingTypeFromInteger 87   = SpawnSpot -- Not quite sure what this is yet, but the docs say "Where Todd McFarelene's guy appears"
thingTypeFromInteger 2005 = Chainsaw
thingTypeFromInteger 2001 = Shotgun
thingTypeFromInteger 82   = DoubleBarreledShotgun
thingTypeFromInteger 2002 = Chaingun
thingTypeFromInteger 2003 = RocketLauncher
thingTypeFromInteger 2004 = Plasmagun
thingTypeFromInteger 2006 = BFG9000
thingTypeFromInteger 2007 = AmmoClip
thingTypeFromInteger 2008 = ShotgunShells
thingTypeFromInteger 2010 = Rocket
thingTypeFromInteger 2047 = CellCharge
thingTypeFromInteger 2048 = BoxOfAmmo
thingTypeFromInteger 2049 = BoxOfShells
thingTypeFromInteger 2046 = BoxOfRockets
thingTypeFromInteger 17   = CellChargePack
thingTypeFromInteger 8    = Backpack
thingTypeFromInteger 2011 = Stimpack
thingTypeFromInteger 2012 = Medikit
thingTypeFromInteger 2014 = HealthPotion
thingTypeFromInteger 2015 = SpiritArmor
thingTypeFromInteger 2018 = SecurityArmor --Green Armor
thingTypeFromInteger 2019 = CombatArmor   --Blue Armor
thingTypeFromInteger 83   = MegaSphere
thingTypeFromInteger 2013 = SoulSphere
thingTypeFromInteger 2022 = Invulnerability
thingTypeFromInteger 2023 = BeserkPack
thingTypeFromInteger 2024 = Invisibility
thingTypeFromInteger 2025 = RadiationSuit
thingTypeFromInteger 2026 = ComputerMap
thingTypeFromInteger 2045 = LightAmplificationGoggles
thingTypeFromInteger 5    = BlueKeyCard
thingTypeFromInteger 40   = BlueSkullKey
thingTypeFromInteger 13   = RedKeyCard
thingTypeFromInteger 38   = RedSkullKey
thingTypeFromInteger 6    = YellowKeyCard
thingTypeFromInteger 39   = YellowSkullKey
thingTypeFromInteger 2035 = Barrel
thingTypeFromInteger 72   = HangingBilly -- I have no idea what this is and the docs aren't clear.
thingTypeFromInteger 48   = TallTechnoColumn
thingTypeFromInteger 30   = TallGreenPillar
thingTypeFromInteger 32   = TallRedPillar
thingTypeFromInteger 31   = ShortGreenPillar
thingTypeFromInteger 36   = ShortGreenPillarWithBeatingHeart
thingTypeFromInteger 33   = ShortRedPillar
thingTypeFromInteger 37   = ShortRedPillarWithSkull
thingTypeFromInteger 47   = Stalagmite
thingTypeFromInteger 43   = BurntGreyTree
thingTypeFromInteger 54   = LargeBrownTree
thingTypeFromInteger 2028 = FloorLamp
thingTypeFromInteger 85   = TallTechnoLamp
thingTypeFromInteger 86   = ShortTechnoLamp
thingTypeFromInteger 34   = Candle
thingTypeFromInteger 35   = Candleabra
thingTypeFromInteger 44   = TallBlueFireStick
thingTypeFromInteger 45   = TallGreenFireStick
thingTypeFromInteger 46   = TallRedFireStick
thingTypeFromInteger 55   = ShortBlueFireStick
thingTypeFromInteger 56   = ShortGreenFireStick
thingTypeFromInteger 57   = ShortRedFireStick
thingTypeFromInteger 70   = BurningBarrel
thingTypeFromInteger 41   = EvilEyeSymbol
thingTypeFromInteger 42   = FlamingSkullRock
thingTypeFromInteger 49   = HangingVictimTwitching
thingTypeFromInteger 63   = HangingVictimTwitching2
thingTypeFromInteger 50   = HangingVictimArmsSpread
thingTypeFromInteger 59   = HangingVictimArmsSpread
thingTypeFromInteger 52   = HangingPairOfLegs
thingTypeFromInteger 60   = HangingPairOfLegs2
thingTypeFromInteger 51   = HangingVictim1Legged
thingTypeFromInteger 61   = HangingVictim1Legged2
thingTypeFromInteger 53   = HangingLeg
thingTypeFromInteger 62   = HangingLeg2
thingTypeFromInteger 73   = HangingVictimNoGuts
thingTypeFromInteger 74   = HangingVictimNoGutsBrain
thingTypeFromInteger 75   = HangingTorsoLookingDown
thingTypeFromInteger 76   = HangingTorsoOpenSkull
thingTypeFromInteger 77   = HangingTorsoLookingUp
thingTypeFromInteger 78   = HangingTorsoNoBrain
thingTypeFromInteger 25   = ImpaledHuman
thingTypeFromInteger 26   = TwitchingImpaledHuman
thingTypeFromInteger 27   = SkullOnPole
thingTypeFromInteger 28   = FiveSkullShishKebab
thingTypeFromInteger 29   = PileOfSkullsAndCandles
thingTypeFromInteger 10   = BloodyMessExplodedPlayer
thingTypeFromInteger 12   = BloodyMessExplodedPlayer
thingTypeFromInteger 24   = PoolOfGuts
thingTypeFromInteger 79   = PoolOfBlood
thingTypeFromInteger 80   = PoolOfBlood
thingTypeFromInteger 81   = PoolOfBrains
thingTypeFromInteger 15   = DeadPlayer
thingTypeFromInteger 18   = DeadFormerHuman
thingTypeFromInteger 19   = DeadFormerSeargent
thingTypeFromInteger 20   = DeadImp
thingTypeFromInteger 21   = DeadDemon
thingTypeFromInteger 22   = DeadCacodemon
thingTypeFromInteger 23   = DeadLostSoulInvisible

data Thing =
  Thing
  {
    thingsPosX  :: I.Int16,
    thingsPosY  :: I.Int16,
    thingsAngle :: I.Int16,
    thingsType  :: ThingType
  }

