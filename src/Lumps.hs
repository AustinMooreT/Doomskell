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

