module Lib where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary.Get as G

{-| Functions extending Binary's Get-}

getChar_ :: G.Get Char
getChar_ = G.getWord8 >>= \char -> return $! head . C8.unpack . BS.singleton $ char

getString :: Integer -> G.Get String
getString n = getList n getChar_

getList :: Integer -> G.Get a -> G.Get [a]
getList n t = goGet n []
  where
    goGet 0 s = return $! reverse s
    goGet m s = t >>= \item -> goGet (m - 1) (item:s)

{-^ Ends extended get functions-}

{- | Functions for String manipulation.-}

trimPaddedString :: String -> String
trimPaddedString = filter (/= '\0')

{- ^ End String manip-}
