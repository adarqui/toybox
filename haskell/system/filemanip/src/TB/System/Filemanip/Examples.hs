{-# LANGUAGE OverloadedStrings #-}

module TB.System.Filemanip.Examples (
  findRegularFiles,
  findFilesOwnedBy,
  findLargeFiles,
  findRecentlyAddedFiles
) where

import           Control.Monad
import qualified Data.ByteString.Char8    as S
import qualified Data.ByteString.Lazy     as L
import           System.FilePath
import           System.FilePath.Find
import           System.FilePath.Glob
import           System.FilePath.Manip
import           System.Posix             hiding (fileMode, fileOwner, fileSize,
                                           modificationTime)
import           System.PosixCompat.Types
import           Text.Regex.Posix         ((=~))

findRegularFiles :: FilePath -> IO [FilePath]
findRegularFiles path = find always (fileType ==? RegularFile) path

-- | Find files by username
--
-- findFilesOwnedBy "x" "/tmp"
findFilesOwnedBy :: String -> FilePath -> IO [FilePath]
findFilesOwnedBy user path = do
  u <- getUserEntryForName user
  find always (fileOwner ==? userID u) path

-- | Find files larger than sz bytes
--
-- findLargeFiles 100000000 "/Users"
findLargeFiles :: COff -> FilePath -> IO [FilePath]
findLargeFiles sz path = do
  find always (fileSize >=? sz) path

-- | Find recently added files
--
findRecentlyAddedFiles :: FilePath -> IO [FilePath]
findRecentlyAddedFiles path = do
  t <- epochTime
  find always (modificationTime >=? elapsed t) path
  where
    elapsed t = t - (60 * 60)
