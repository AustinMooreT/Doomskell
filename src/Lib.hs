module Lib where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G
import qualified Data.Int as I
import qualified Data.Word as W

{-| Functions extending Binary's Get-}

-- | Extends binary's get to little endian 8 bit char values.
getCharle :: G.Get Char
getCharle = do
  chr <- G.getWord8
  return $! head . C8.unpack . BS.singleton $ chr
-- ^

-- | Extends binary's get to n sized strings
getStringle :: Integer -> G.Get String
getStringle n = goGet n []
  where
    goGet 0 s = return $! reverse s
    goGet m s = do
      char <- getCharle
      goGet (m - 1) (char:s)
-- ^

{-^ Ends extended get functions-}
