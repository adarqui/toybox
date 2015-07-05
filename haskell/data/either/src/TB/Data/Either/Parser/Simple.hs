module TB.Data.Either.Parser.Simple (
  parseString,
  parseDigit,
  parseHexString,
  parseHex
) where

import           Data.Char

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
parseDigit :: Char -> Either String Integer
parseDigit c
  | isDigit c = Right (fromIntegral (digitToInt c) :: Integer)
  | otherwise = Left "parse error"

-- | Parse a string of digits
--
-- >>> parseString "1234"
-- Right [1,2,3,4]
--
parseString :: String -> Either String [Integer]
parseString s = do
  mapM parseDigit $ dropWhile (== '0') s

-- Hex Strings

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
parseHex :: Char -> Either String Hex
parseHex c
  | isDigit c = Right $ HexDigit $ digitToInt c
  | lower >= 'a' && lower <= 'f' = Right $ HexChar c
  | otherwise = Left "parse error"
  where
    lower = toLower c

-- | Parse hex digits from a string
--
-- >>> parseHexString "a9"
-- Right [HexChar 'a',HexDigit 9]
--
parseHexString :: String -> Either String [Hex]
parseHexString s = do
  mapM parseHex s
