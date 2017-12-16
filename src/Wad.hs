module Wad (Wad,
            Lump,
            LumpInfo,
            WadType) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Data.Word as W
import Lib

{- | Set of data types and functions for retrieving the header from a binary wad file-}

-- | Type of wad file
data WadType = IWAD | PWAD | NotWad
  deriving (Show, Eq)
-- ^

-- | Convert a given String to the proper WadType
wadTypeFromStr :: String -> WadType
wadTypeFromStr "IWAD" = IWAD
wadTypeFromStr "PWAD" = PWAD
wadTypeFromStr _ = NotWad
-- ^

-- | Retuns a get monad representing the wad type.
getWadType :: G.Get WadType
getWadType = do
  str <- getStringle 4
  return $! wadTypeFromStr str
-- ^

-- | Header of the wadfile
data Header =
  Header
  {
    wadtype :: WadType,
    numLumps :: I.Int32,
    infoTablePtr :: I.Int32
  }
  deriving (Show, Eq)
-- ^

-- | returns a get monad representing the header of a wad file
getWadHeader :: G.Get Header
getWadHeader = do
  wadtype <- getWadType
  numLumps <- G.getInt32le
  infoTablePtr <- G.getInt32le
  return $! Header wadtype numLumps infoTablePtr
-- ^

-- | Checks wether or not the file contains a valid header for a doom file
checkForValidHeader :: Header -> Bool
checkForValidHeader (Header NotWad _ _) = False
checkForValidHeader _ = True
-- ^

{-^ Headers -}

-----

{- | Set of data types and functions to get lumps out of given wad file-}

-- | Information about the lump at the given ptr
data LumpInfo =
  LumpInfo
  {
    lumpPtr :: I.Int32,
    lumpSize :: I.Int32,
    lumpName :: String
  }
  deriving (Show, Eq)
-- ^

-- | returns a get monad representing LumpInfo 
getLumpInfo :: G.Get LumpInfo
getLumpInfo = do
  ptr <- G.getInt32le
  size <- G.getInt32le
  str <- getStringle 8
  return $! LumpInfo ptr size (filter (\x -> x /= '\0') str) --Filter out null terminator from names
-- ^

-- | Table of info about lumps
type InfoTable = [LumpInfo]
-- ^

-- | return get monad containing the infotable
getInfoTable :: Integer -> G.Get InfoTable
getInfoTable n = goGet n []
  where
  goGet 0 s = return $! reverse s
  goGet m s = do
    lumpInfo <- getLumpInfo
    goGet (m - 1) (lumpInfo:s)
-- ^

-- | get's an info table specified by a provided header
getInfoTableFromHeader :: Header -> G.Get InfoTable
getInfoTableFromHeader (Header NotWad _ _) = return []
getInfoTableFromHeader (Header _ numLumps offset) = (G.skip . fromIntegral . toInteger $ offset)
                                                    >>= (\_ -> getInfoTable . toInteger $ numLumps)
-- ^

-- | represents a lump of data in the wad file
data Lump =
  Lump
  {
    info :: LumpInfo,
    bytes :: BSL.ByteString
  }
  deriving (Show, Eq)
-- ^

-- | get a lump given some lumpinfo
getLump :: LumpInfo -> G.Get Lump
getLump (LumpInfo ptr size name) = do
  offset <- G.skip . fromIntegral . toInteger $ ptr
  byts <- G.getLazyByteString . fromIntegral . toInteger $ size
  return $! Lump (LumpInfo ptr size name) byts
-- ^

-- | get list of lumps from InfoTable
getLumps :: InfoTable -> [G.Get Lump]
getLumps s = map getLump s
-- ^

{-^-}

-------

{- | Functions and data types for parsing a wadfile into lumps -}

-- | data structure representing a parsed wad file
data Wad = Wad
         {
           header :: Header,
           lumps :: [Lump]
         }
  deriving (Show, Eq)
-- ^

-- | read a wad file at a given directory into an io monad
readWad :: String -> IO Wad
readWad s = headr >>= (\x -> lums >>= (\y -> return $! Wad x y))
  where
    file = BSL.readFile s
    headr = file >>= (\x -> return $! G.runGet getWadHeader x)
    lumpInfoTable = file >>= (\x -> headr >>= (\y -> return $! G.runGet (getInfoTableFromHeader y) x))
    lums = file >>= (\x -> lumpInfoTable >>= (\y -> return $! map ($ x) (map (G.runGet) (getLumps y))))
-- ^
{-^-}
