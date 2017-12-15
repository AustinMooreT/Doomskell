module Lumps () where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Wad as W
import qualified Data.Word as DW

{- | PLAYPAL Lump Reading
PLAYPAL refers to the color pallette used
14 palettes with 256 rgb triples in each palette-}

type Color = DW.Word8

data RGB =
  RGB
  {
    red :: Color,
    green :: Color,
    blue :: Color
  }

getRGB :: G.Get RGB
getRGB = do
  r <- G.getWord8le
  g <- G.getWord8le
  b <- G.getWord8le
  return $! RGB r g b

type Palette = [RGB]

getPalette :: G.Get Palette
getPalette = goGet 256 []
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      rgb <- getRGB
      goGet (n - 1) (rgb:s)

type PLAYPAL = [Palette]

getPLAYPAL :: G.Get PLAYPAL
getPLAYPAL = goGet 14 []
  where
    goGet 0 s = return $! reverse s
    goGet n s = do
      palet <- getPalette
      goGet (n - 1) (palet:s)

{-^-}


{- | Picture format -}

data Header =
  Header
  {
    
  }

{-^-}
