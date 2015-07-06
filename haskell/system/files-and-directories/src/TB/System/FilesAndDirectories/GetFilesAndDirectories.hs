module TB.System.FilesAndDirectories.GetFilesAndDirectories (
  getSources,
  getSources',
  getFilesAndDirectories,
  getFilesAndDirectories'1,
  getFilesAndDirectories'2,
  getFilesAndDirectories'3,
  getFilesAndDirectories'4,
  getFilesAndDirectories'5
) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           System.Directory
import           System.FilePath

getSources :: FilePath -> IO [FilePath]
getSources = getSources' getFilesAndDirectories

getSources' :: (FilePath -> IO ([FilePath], [FilePath])) -> FilePath -> IO [FilePath]
getSources' act path = filter (isSuffixOf ".hs") <$> go path
  where
    go dir = do
      (dirs, files) <- act dir
      (files ++) . concat <$> mapM go dirs

-- | ekmett code
--
-- returns a tuple of (directories, filenames)
getFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories dir = do
  c <- map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c



-- variations of ekmett's code

getFilesAndDirectories'1 :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories'1 dir = do
  c <- map (dir `combine`) . filter(`notElem` ["..", "."]) <$> getDirectoryContents dir
  (,) <$> filterM doesDirectoryExist c <*> filterM doesFileExist c



getFilesAndDirectories'2 :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories'2 dir = do
  c <- map (dir `combine`) . filter(`notElem` ["..", "."]) <$> getDirectoryContents dir
  liftM2 (,) (filterM doesDirectoryExist c) (filterM doesFileExist c)



getFilesAndDirectories'3 :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories'3 dir = do
  c <- getDirectoryContents dir >>= return . map (dir `combine`) . filter(`notElem` ["..", "."])
  liftM2 (,) (filterM doesDirectoryExist c) (filterM doesFileExist c)



getFilesAndDirectories'4 :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories'4 dir = do
  c <- liftM (mapM (dir `combine`) . filter(`notElem` ["..", "."])) $ getDirectoryContents dir
  liftM2 (,) (filterM doesDirectoryExist c) (filterM doesFileExist c)



getFilesAndDirectories'5 :: FilePath -> IO ([FilePath], [FilePath])
getFilesAndDirectories'5 dir = do
  c <- fmap (map (dir `combine`) . filter(`notElem` ["..", "."])) (getDirectoryContents dir)
  fmap (,) (filterM doesDirectoryExist c) <*> (filterM doesFileExist c)
