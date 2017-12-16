module Lumps () where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Wad as W
import qualified Data.Word as DW
import qualified Data.Array.Unboxed as ARR
import Lib

{- | PLAYPAL
 - PLAYPAL refers to the color pallette used
 - 14 palettes with 256 rgb triples in each palette
 -}

-- | A color is just a single color type represente by an 8 bit value
type Color = DW.Word8
-- ^

-- | rbg is a three color combo
data RGB =
  RGB
  {
    red :: Color,
    green :: Color,
    blue :: Color
  }
-- ^

-- | get's an rgb triple from a bytestring
getRGB :: G.Get RGB
getRGB = do
  r <- G.getWord8
  g <- G.getWord8
  b <- G.getWord8
  return $! RGB r g b
-- ^

-- | represents a palette of rgbs the size will always be 256
type Palette = ARR.Array Integer RGB
-- ^

-- | get's a palette of 256 rgb values from a byte string.
getPalette :: G.Get Palette
getPalette = getList 256 getRGB >>= (\x -> return $! ARR.listArray (0,255) x)
-- ^

-- | Type representing the PLAYPAL lup
type PLAYPAL = ARR.Array Integer Palette
-- ^

-- | Extracts a playpal lump from a bytestring.
getPLAYPAL :: G.Get PLAYPAL
getPLAYPAL = getList 14 getPalette >>= (\x -> return $! ARR.listArray (0,14) x)
-- ^

-- | turns a bytestring into a PLAYPAL structure
readPLAYPAL :: BSL.ByteString -> PLAYPAL
readPLAYPAL bs = G.runGet getPLAYPAL bs
-- ^

{-^ End PLAYPAL-}

----

{- | COLORMAP
 - Think of the colormap lump as a transformation to different brightness levels.
 - It maps a specific brightness level to a color in PLAYPAL.
 -}

type Index = DW.Word8

getIndex :: G.Get Index
getIndex = G.getWord8

type Map = ARR.Array Integer Index

getMap :: G.Get Map
getMap = getList 256 getIndex >>= (\x -> return $! ARR.listArray (0,255) x)

type COLORMAP = ARR.Array Integer Map

getCOLORMAP :: G.Get COLORMAP
getCOLORMAP = getList 34 getMap >>= (\x -> return $! ARR.listArray (0,33) x)

{-^ End COLORMAP-}

------

{- | TEXTUREN
 - There are two types of TEXTURE lumps one in Doom1 wad (shareware), and one in Doom wad (paid)
 - They each contain the set of wall textures used in the game.
 -}

data Patch =
  Patch
  {
    ptchXOffset :: I.Int16,
    ptchYOffset :: I.Int16,
    pnameIndex :: I.Int16
  }

getPatch :: G.Get Patch
getPatch = do
  offx <- G.getInt16le
  offy <- G.getInt16le
  indx <- G.getInt16le
  _ <- G.skip 4 -- Two integers that are never used. If I need them check the unofficial docs.
  return $! Patch offx offy indx

getPatches :: Integer -> G.Get [Patch]
getPatches n = getList n getPatch

data Texture =
  Texture
  {
    tname :: String,
    twidth :: I.Int16,
    theight :: I.Int16,
    patches :: [Patch]
  }

getTexture :: G.Get Texture
getTexture = do
  name <- getStringle 8
  _ <- G.skip 4 -- Two empty integers again.
  widt <- G.getInt16le
  heig <- G.getInt16le
  _ <- G.skip 4 -- Two empty integers again.
  nPtch <- G.getInt16le
  (return (Texture  name  widt  heig)) >>= (\x -> (getPatches . toInteger $ nPtch) >>= (\y -> return $! x y))

type TEXTURE = [Texture]

readTEXTURE :: BSL.ByteString -> TEXTURE
readTEXTURE bs = (map ($ bs) (map (G.runGet) (map ($ getTexture) (map (>>) (map (G.skip) ptrs)))))
  where
    num = G.runGet G.getInt32le bs
    ptrs = map (fromIntegral . toInteger) (G.runGet (getList (fromIntegral . toInteger $ num) G.getInt32le) bs)

{-^ End TEXTUREN-}

--------

{- | PNAMES
 - PNAMES represent names used by walls in doom
 -}

type PNAME = String

getPNAME :: G.Get PNAME
getPNAME = getStringle 8

type PNAMES = [String]

getPNAMES :: G.Get PNAMES
getPNAMES = do
  numNames <- G.getInt32le
  names <- getList (fromIntegral . toInteger $ numNames) getPNAME
  return names

{-^ End PNAMES-}

-------

{- | Picture format -}

data Header =
  Header
  {
    picwidth :: I.Int16,
    picheight :: I.Int16,
    lefOffset :: I.Int16,
    topOffset :: I.Int16
  }

getPictureHeader :: G.Get Header
getPictureHeader = do
  w <- G.getInt16le
  h <- G.getInt16le
  l <- G.getInt16le
  t <- G.getInt16le
  return $! Header w h l t



{-^-}
