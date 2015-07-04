{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import System.Environment
import System.Process
import System.Cmd
import Data.List

main :: IO ()
main = do
 as <- getArgs
 mapM_ process as

process :: String -> IO ()
process file = do
 cts <- readFile file
 let tests = getTests cts
 if (null tests) then
  putStrLn (file ++ ": no properties to check")
 else do
  writeFile "gen/Script.hs" $ unlines (concatMap makeTest tests)
  system ("ghci -v0 < script")
  return ()

getTests :: String -> [String]
getTests cts =
 nub $ filter ("prop_" `isPrefixOf`) $ map (fst . head . lex) $ lines cts

makeTest :: String -> [String]
makeTest p = do
 ["module Script (run) where\n",
  "import Test.QuickCheck\n",
  "import Props\n",
  "run :: IO ()\n",
  "run = do\n",
  " putStr \"" ++ p ++ ": \"",
  " quickCheck " ++ p]
