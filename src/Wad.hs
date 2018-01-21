module Wad () where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Data.Word as DW
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
    wadHeaderType :: WadType,
    wadHeaderLumpCount :: I.Int32,
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
    lumpSize :: I.Int32,
    lumpName :: String
  }
  deriving (Show, Eq)

getLumpInfo :: G.Get LumpInfo
getLumpInfo = LumpInfo <$> G.getInt32le <*> G.getInt32le <*> (getString lumpNameSize >>=
                                                              (\str -> return $! trimPaddedString str))
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
               (\bytes -> return $! Lump info bytes)

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
           wadLumps :: [Lump]
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
 - PLAYPAL represents the colors used by DOOM in representing pixel colors.
 - The PLAYPAL contains 14 palettes with 256 3 byte RGB triples in each palette.
 -}

type ColorChannel = DW.Word8

data RGB =
  RGB
  {
    redChannel :: ColorChannel,
    greenChannel :: ColorChannel,
    blueChannel :: ColorChannel
  }

getRGB :: G.Get RGB
getRGB = RGB <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

type Palette = ARR.Array Integer RGB

getPalette :: G.Get Palette
getPalette = getList 256 getRGB >>= \palette -> return $! ARR.listArray (0,255) palette

type PLAYPAL = ARR.Array Integer Palette

getPLAYPAL :: G.Get PLAYPAL
getPLAYPAL = getList 14 getPalette >>= \playpal -> return $! ARR.listArray (0,14) playpal

isPLAYPAL :: Lump -> Bool
isPLAYPAL = isLUMP "PLAYPAL"

{-^ End PLAYPAL -}

------------------

{- |
 - COLORMAP is a lump containing brightness and color shift information.
 - Each index maps a specific byte to a specific palette in the PLAYPAL.
 - Each color map is 256 byte long table.
 - There are a total of 34 color maps indexed from 0 - 33
 - 0 - 31 the first 32 maps index brightness in decreasing order; where 0 is max brightness and 31 is complete darkness.
 - 32 is an invunreablity powerup and 33 is all black.
 -}

type Index = DW.Word8

getIndex :: G.Get Index
getIndex = G.getWord8

type Map = ARR.Array Integer Index

getMap :: G.Get Map
getMap = getList 256 getIndex >>= \x -> return $! ARR.listArray (0,255) x

type COLORMAP = ARR.Array Integer Map

getCOLORMAP :: G.Get COLORMAP
getCOLORMAP = getList 34 getMap >>= \x -> return $! ARR.listArray (0,33) x

isCOLORMAP :: Lump -> Bool
isCOLORMAP = isLUMP "COLORMAP"

{-^ End COLORMAP -}

-------------------

{- |
 - ENDOOM is a lump containing the endscreen for when you exit the game.
 - It is just 4000 bytes representing an 80 by 25 ansi screen.
 -}

type AnsiColor = DW.Word8

data AnsiCharacter =
  AnsiCharacter
  {
    ansiChar :: Char,
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
    patchXOffset :: I.Int16,
    patchYOffset :: I.Int16,
    patchNameIndex :: I.Int16
  }

getPatch :: G.Get Patch
getPatch = Patch <$> G.getInt16le <*> G.getInt16le <*> G.getInt16le

getPatches :: Integer -> G.Get [Patch]
getPatches n = getList n getPatch

data Texture =
  Texture
  {
    textureName :: String,
    textureWidth :: I.Int16,
    textureHeight :: I.Int16,
    texturePatches :: [Patch]
  }

getTexture :: G.Get Texture
getTexture = getString 8 >>=
              \name -> G.skip 4 >>=
              \_ -> G.getInt16le >>=
              \width -> G.getInt16le >>=
              \height -> G.skip 4 >>=
              \_ -> G.getInt16le >>=
              \numPatches -> (getPatches $! toInteger numPatches) >>=
              \patches -> return $! Texture name width height patches

type TEXTURE = [Texture]

getTEXTURE :: G.Get TEXTURE
getTEXTURE = G.lookAhead G.getInt32le >>=
             \nums -> G.lookAhead (getList  (fromIntegral $! toInteger nums) G.getInt32le) >>=
             \ptrs -> (return $! map (fromIntegral . toInteger) ptrs) >>=
             \offsets -> mapM (\x -> G.lookAhead (G.skip x >>= (\_ -> getTexture))) offsets

isTEXTURE :: Lump -> Bool
isTEXTURE l = or [(isLUMP "TEXTURE1" l), (isLUMP "TEXTURE2" l)]

{-^ End TEXTUREN -}

-------------------

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
            \names -> return names

{-^ End PNAMES-}

----------------

{- | Picture format -}

data PictureHeader =
  PictureHeader
  {
    pictureWidth :: I.Int16,
    pictureHeight :: I.Int16,
    picXOffset :: I.Int16,
    picYOffset :: I.Int16
  }

getPictureHeader :: G.Get PictureHeader
getPictureHeader = PictureHeader <$> G.getInt16le <*> G.getInt16le <*> G.getInt16le <*> G.getInt16le

data ColumnHeader =
  ColumnHeader
  {

  }

data AnimationFrames = AniA | AniB | AniC | AniD | AniE | AniF | AniG | AniH | AniI | AniJ | AniK | AniL | AniM | AniN |
                       AniO | AniP | AniQ | AniR | AniS | AniT | AniU | AniV | AniW | AniX | AniY | AniZ

{-^ End Picture Format-}

------------------------

{- |
 - THINGS represent players, monsters, pick-ups, and projectiles.
 - Internally these things are called actors.
 - -}

data ThingTypeInfo =
  ThingTypeInfo
  {
    thingTypeInfoIndex :: Integer,
    thingTypeInfoDoomVer :: Integer
    
  }

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
  SuperShotgun                     |
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
  HangingVictim1Leg                |
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
  HangingVictim1Legged             |
  HangingPairOfLegs2               |
  HangingLeg2                      |
  UnknownThing Integer

data THINGS =
  THINGS
  {
    thingsPosX :: I.Int16,
    thingsPosY :: I.Int16,
    thingsAngle :: I.Int16,
    thingsType :: ThingType
  }

