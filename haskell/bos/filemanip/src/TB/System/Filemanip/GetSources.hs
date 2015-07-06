-- ekmett's original functions can be found here:
-- https://github.com/ekmett/bound/blob/master/tests/doctests.hs

module TB.System.Filemanip.GetSources (
  getSources
) where

import           System.FilePath.Find

-- | Return all regular haskell source files, recursively scanning the provided FilePath
--
-- >>> getSources "src" >>= return . not . null
-- True
--
getSources :: FilePath -> IO [FilePath]
getSources = find always (extension ==? ".hs" &&? fileType ==? RegularFile)
