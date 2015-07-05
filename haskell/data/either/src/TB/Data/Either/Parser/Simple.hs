{-# LANGUAGE OverloadedStrings #-}

module TB.Data.Either.Parser.Simple (
  parseText,
  parseDigit,
  parseHexText,
  parseHex
) where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

data Hex =
    HexDigit Int
  | HexChar Char
  deriving (Eq, Show)

-- | Parse a digit from a character
--
-- >>> parseDigit '0'
-- Right 0
--
-- >>> parseDigit 'm'
-- Left "parse error"
--
parseDigit :: Char -> Either Text Integer
parseDigit c
  | isDigit c = Right (fromIntegral (digitToInt c) :: Integer)
  | otherwise = Left "parse error"

-- | Parse a string of digits
--
-- >>> parseText "1234"
-- Right [1,2,3,4]
--
parseText :: Text -> Either Text [Integer]
parseText = mapM parseDigit . T.unpack . T.dropWhile (== '0')

-- Hex Texts

-- | Parse a hex digit from a character
--
-- >>> parseHex 'a'
-- Right (HexChar 'a')
--
-- >>> parseHex '1'
-- Right (HexDigit 1)
--
-- >>> parseHex 'g'
-- Left "parse error"
--
parseHex :: Char -> Either Text Hex
parseHex c
  | isDigit c = Right $ HexDigit $ digitToInt c
  | lower >= 'a' && lower <= 'f' = Right $ HexChar c
  | otherwise = Left "parse error"
  where
    lower = toLower c

-- | Parse hex digits from a string
--
-- >>> parseHexText "a9"
-- Right [HexChar 'a',HexDigit 9]
--
parseHexText :: Text -> Either Text [Hex]
parseHexText = mapM parseHex . T.unpack
