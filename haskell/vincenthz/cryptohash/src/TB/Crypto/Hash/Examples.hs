{-# LANGUAGE OverloadedStrings #-}

module TB.Crypto.Hash.Examples (
  sha1File
) where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash.SHA1
import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Data.Char
import           Data.Word
import           Numeric

-- | SHA1 a file
sha1File :: FilePath -> IO String
sha1File = liftM (bsToHex . hash) . B.readFile

-- | Convert a bytestring to a hex string
--
-- >>> bsToHex "hi"
-- "6968"
bsToHex :: ByteString -> String
bsToHex = concat . B.foldl (\acc c -> (showHex c "") : acc) []
