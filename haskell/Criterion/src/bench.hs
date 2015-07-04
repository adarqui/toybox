import Common.Sums
 (sumFoldl, sumFoldl', sumFoldr)

import Criterion.Config
 (Config(..), PrintExit(..), defaultConfig, cfgPerformGC, ljust)

import Criterion.Main
 (parseArgs, defaultOptions, defaultMain, bgroup, bench, nf, bcompare)

import Criterion
 (runAndAnalyse)

import System.Environment
 (getArgs)

import Control.Monad.Trans
 (liftIO)

import Criterion.IO.Printf
 (note, printError)

import Criterion.Environment
 (measureEnvironment)

import Criterion.Monad
 (Criterion, withConfig)

import Criterion.Types
 (Benchmarkable(..), Benchmark(..), Pure, bench, benchNames, bgroup, nf, nfIO, whnf, whnfIO)

import Data.List
 (isPrefixOf, sort, foldl')

import Data.Monoid
 (Monoid(..), Last(..))

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)

-- fold/sum --
{-
fl :: (Num a) => [a] -> a
fl xs = foldl (+) 0 xs

fl' ::  (Num a) => [a] -> a
fl' xs = foldl' (+) 0 xs

fr ::  (Num a) => [a] -> a
fr xs = foldr (+) 0 xs

s :: (Num a) => [a] -> a
s xs = sum xs
-}

config :: Config
config = defaultConfig {
  cfgPerformGC = ljust True,
  cfgResults = report "results",
  cfgReport = report "report",
  cfgSummaryFile = report "summary",
  cfgCompareFile = report "compare",
  cfgJUnitFile = report "junit"
 }
 where
  report s = ljust $ "./sample-reports/" ++ s

usage :: String
usage = "usage: ./bench <num_list_size>"

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (n:ns) -> do
   let num = read n :: Integer
   benchFolds ns num
   return ()
  _ -> putStrLn usage

benchFolds :: [String] -> Integer -> IO ()
benchFolds argv n = do
 defaultMainWith' argv config (return ()) [
  bgroup "sums/nf" $ benchmarksNF n,
  bgroup "sums/whnf" $ benchmarksWHNF n,
  bgroup "comparing sums nf/whnf" $ [compareFolds n]
  ]
 return ()

compareFolds :: Integer -> Benchmark
compareFolds n = do
 bcompare $ [bgroup "nf" $ benchmarksNF n, bgroup "whnf" $ benchmarksWHNF n]

benchmarksNF :: Integer -> [Benchmark]
benchmarksNF n =
 [
  bench "foldl" $ nf sumFoldl [0..n],
  bench "foldl'" $ nf sumFoldl' [0..n],
  bench "foldr" $ nf sumFoldr [0..n],
  bench "sum" $ nf sum [0..n]
 ]

benchmarksWHNF :: Integer -> [Benchmark]
benchmarksWHNF n =
 [
  bench "foldl" $ whnf sumFoldl [0..n],
  bench "foldl'" $ whnf sumFoldl' [0..n],
  bench "foldr" $ whnf sumFoldr [0..n],
  bench "sum" $ whnf sum [0..n]
 ]

defaultMainWith' :: [String] -> Config -> Criterion () -> [Benchmark] -> IO ()
defaultMainWith' argv defCfg prep bs = do
  (cfg, args) <- parseArgs defCfg defaultOptions argv
  withConfig cfg $
   if cfgPrintExit cfg == List
    then do
      _ <- note "Benchmarks:\n"
      mapM_ (note "  %s\n") (sort $ concatMap benchNames bs)
    else do
      case getLast $ cfgSummaryFile cfg of
        Just fn -> liftIO $ writeFile fn "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB\n"
        Nothing -> return ()
      env <- measureEnvironment
      let shouldRun b = null args || any (`isPrefixOf` b) args
      prep
      runAndAnalyse shouldRun env $ BenchGroup "" bs
