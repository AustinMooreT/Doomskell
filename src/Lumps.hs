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





------



--------

{- | PNAMES
 - PNAMES represent names used by walls in doom
 -}

type PNAME = String

getPNAME :: G.Get PNAME
getPNAME = getString 8

type PNAMES = [String]

getPNAMES :: G.Get PNAMES
getPNAMES = do
  numNames <- G.getInt32le
  names <- getList (fromIntegral . toInteger $ numNames) getPNAME
  return names

{-^ End PNAMES-}

-------

{- | Picture format -}

data PictureHeader =
  PictureHeader
  {
    picwidth :: I.Int16,
    picheight :: I.Int16,
    pxOffset :: I.Int16,
    pyOffset :: I.Int16
  }

getPictureHeader :: G.Get PictureHeader
getPictureHeader = do
  w <- G.getInt16le
  h <- G.getInt16le
  l <- G.getInt16le
  t <- G.getInt16le
  return $! PictureHeader w h l t

data ColumnHeader =
  ColumnHeader
  {
    cyOffset :: I.Int
  }

{-^-}

