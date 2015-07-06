{-# LANGUAGE RankNTypes #-}
module Main (
 main
) where

import System.Environment
 (getArgs)

import Text.CSV
 (CSV(..), parseCSVFromFile, parseCSV)

import Prelude hiding (getContents)

import Pipes
import qualified Pipes.Prelude as P

import Control.Monad
 (liftM, unless)

import Data.List
 (intersperse)

import qualified System.IO as IO

fromHandle :: (MonadIO m) => IO.Handle -> Producer' String m ()
fromHandle h = go
 where
  go = do
   eof <- liftIO $ IO.hIsEOF h
   unless eof $ do
    str <- liftIO $ IO.hGetContents h
    yield str
    go

stdin :: (MonadIO m) => Producer' String m ()
stdin = fromHandle IO.stdin

collect :: (Monad m) => Producer a m () -> m (Maybe [a])
collect p = do
 x <- next p
 return $ case x of
  (Left _) -> Nothing
  (Right (a, _)) -> Just $ [a]

readStdin :: IO String
readStdin = do
 r <- runEffect $ collect stdin
 case r of
  Nothing -> return ""
  (Just s) -> do
   return $ concat s

transformResult lr =
 case lr of
  (Left _) -> Nothing
  (Right v) -> Just v

parseArg :: String -> IO (Maybe CSV)
parseArg arg = do
 case arg of
  "-" -> do
   v <- readStdin
   return $ transformResult $ parseCSV arg v
  _ -> do
   r <- parseCSVFromFile arg
   return $ transformResult $ r

parse :: String -> IO ()
parse file = do
 result <- parseArg file
 case result of
  Nothing -> putStrLn "Error"
  (Just v) -> do
   putStrLn $ show $ length v
--   putStrLn $ show v

usage :: String
usage = "usage: ./csv <file.csv>"

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (file:[]) -> parse file
  _ -> putStrLn $ usage

