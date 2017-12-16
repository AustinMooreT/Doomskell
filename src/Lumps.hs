module Lumps () where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Wad as W
import qualified Data.Word as DW
import qualified Data.Array.Unboxed as ARR

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
getPalette = goGet 256 [] >>= (\x -> return $! ARR.listArray (0,255) x)
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      rgb <- getRGB
      goGet (n - 1) (rgb:s)
-- ^

-- | Type representing the PLAYPAL lup
type PLAYPAL = ARR.Array Integer Palette
-- ^

-- | Extracts a playpal lump from a bytestring.
getPLAYPAL :: G.Get PLAYPAL
getPLAYPAL = goGet 14 [] >>= (\x -> return $! ARR.listArray (0,14) x)
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      palet <- getPalette
      goGet (n - 1) (palet:s)
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
getMap = goGet 256 [] >>= (\x -> return $! ARR.listArray (0,255) x)
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      index <- getIndex
      goGet (n - 1) (index:s)

type COLORMAP = ARR.Array Integer Map

getCOLORMAP :: G.Get COLORMAP
getCOLORMAP = goGet 34 [] >>= (\x -> return $! ARR.listArray (0,33) x)
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      map <- getMap
      goGet (n - 1) (map:s)

{-^ End COLORMAP-}

------

{- | Picture format -}

data Header =
  Header
  {
    width :: I.Int16,
    height :: I.Int16,
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
