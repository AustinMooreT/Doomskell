module Wad (Wad,
            Lump,
            LumpInfo,
            WadType) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import Lib

{- |
 - Set of data types and functions for representing and retrieving the header of a DOOM WAD file.
 - The header consists of a 4 byte identity representing wether or not it's an internal WAD or external (IWAD or PWAD)
 - The next 4 bytes are a 32 bit signed integer that keeps count of the number of "lumps" in the WAD
 - The next 4 bytes are a 32 bit signed integer that represents the offset to a lookup table of lumps in the file
 -}

-- | Data structure representing the WADs type
data WadType = IWAD | PWAD | NotWad
  deriving (Show, Eq)
-- ^

-- | Function for getting the WAD type out of a string.
wadTypeFromStr :: String -> WadType
wadTypeFromStr "IWAD" = IWAD
wadTypeFromStr "PWAD" = PWAD
wadTypeFromStr _ = NotWad
-- ^

-- | Retuns a get monad representing the action of extracting the WAD type.
getWadType :: G.Get WadType
getWadType = getStringle 4 >>= (\str -> return $! wadTypeFromStr str)
-- ^

-- | Data structure representing the header of the WAD file.
data WadHeader =
  Header
  {
    wadtype :: WadType, -- Wad type
    numLumps :: I.Int32, -- Number of lumps
    lumpTableOffset :: I.Int32 -- Offset of lump table
  }
  deriving (Show, Eq)
-- ^

-- | Returns a get monad representing the action of extracting the WAD header.
getWadHeader :: G.Get WadHeader
getWadHeader = Header <$> getWadType <*> G.getInt32le <*> G.getInt32le
-- ^

-- | Checks wether or not the file contains a valid header for a DOOM WAD file
checkForValidHeader :: WadHeader -> Bool
checkForValidHeader (Header NotWad _ _) = False
checkForValidHeader _ = True
-- ^

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

-- | Constant representing how many bytes long a lump's name is.
lumpNameSize :: Integer
lumpNameSize = 8
-- ^

-- | Data stucture representing an entry in the lump info table.
data LumpInfo =
  LumpInfo
  {
    lumpOffset :: I.Int32,
    lumpSize :: I.Int32,
    lumpName :: String
  }
  deriving (Show, Eq)
-- ^

-- | returns a get monad representing the action of extracting LumpInfo.
getLumpInfo :: G.Get LumpInfo
getLumpInfo = LumpInfo <$> G.getInt32le <*> G.getInt32le <*> (getStringle lumpNameSize >>=
                                                               (\str -> return $! trimPaddedString str))
-- ^

-- | Data structure represnting the table of LumpInfo.
type InfoTable = [LumpInfo]
-- ^

-- | Returns a get monad representing the action of extracting the InfoTable
getInfoTable :: Integer -> G.Get InfoTable
getInfoTable n = getList n getLumpInfo
-- ^

-- | gets an info table specified by a provided header
getInfoTableFromHeader :: WadHeader -> G.Get InfoTable
getInfoTableFromHeader (Header NotWad _ _) = return []
getInfoTableFromHeader (Header _ numLumps offset) = (G.skip . fromIntegral . toInteger $ offset) >>
                                                    (getInfoTable . toInteger $ numLumps)
-- ^

-- | Data structure representing a generic lump of data in the WAD file.
data Lump =
  Lump
  {
    lumpInfo :: LumpInfo,
    lumpData :: BSL.ByteString
  }
  deriving (Show, Eq)
-- ^

-- | Returns a get monad representing the action of getting a lump provided its entyry in the info table.
getLump :: LumpInfo -> G.Get Lump
getLump info = (G.skip . fromIntegral . toInteger . lumpOffset $ info) >>
               (G.getLazyByteString . fromIntegral . toInteger . lumpSize $ info) >>=
               (\bytes -> return $! Lump info bytes)
-- ^

-- | Returns a list get actions for lumps.
getLumps :: InfoTable -> [G.Get Lump]
getLumps = map getLump
-- ^

{-^ End Lumps-}

---------------

{- |
 - Set of data types and functions for representing and partitioning a DOOM WAD file into its constituent parts.
 - A WAD file can have varying amounts of lumps and lumps with varying names and data.
 - These structures will represent the file with it's header, and a list of generic lumps.
 -}

-- | data structure representing a parsed wad file
data  Wad = Wad
         {
           header :: WadHeader,
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
