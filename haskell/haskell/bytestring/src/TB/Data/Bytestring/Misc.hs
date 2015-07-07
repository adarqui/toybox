module TB.Data.ByteString.Misc (
  bsToHex,
  bsToOctal
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Word
import           Numeric

-- | Convert a bytestring to a hex string
--
-- >>> bsToHex "hi"
-- "6968"
bsToHex :: ByteString -> String
--bsToHex = concat . B.foldl (\acc c -> (showHex c "") : acc) []
bsToHex bs = bsTo (flip showHex "") bs

-- | Convert a bytestring to an octal string
--
-- >>> bsToOctal "hi"
--"151150"
bsToOctal :: ByteString -> String
bsToOctal = bsTo (flip showOct "")

bsTo :: (Word8 -> String) -> ByteString -> String
bsTo f = concat . B.foldl (\acc c -> f c : acc) []
