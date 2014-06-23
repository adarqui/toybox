module AFP08.Argv (
 argv'fib'euler'sum,
 argv'fib'euler'map,
 argv'quicksort,
 usage,
 launch'fib'euler'sum,
 launch'fib'euler'map,
 launch'quicksort'1,
 launch
) where

import AFP08.Elapsed
 (elapsed'Show)

import System.Environment
 (getArgs)

import Control.Parallel
 (pseq)

import Data.Maybe

argv'fib'euler'sum :: IO (Maybe (Int, Int))
argv'fib'euler'sum = do
 argv <- getArgs
 case argv of
  (f:e:[]) -> return $ Just (read f :: Int, read e :: Int)
  _ -> return Nothing

argv'fib'euler'map :: IO (Maybe ([Int], [Int]))
argv'fib'euler'map = do
 argv <- getArgs
 case argv of
  (f:e:[]) -> return $ Just (read f :: [Int], read e :: [Int])
  _ -> return Nothing

argv'quicksort :: IO (Maybe ((Int, Int), Int))
argv'quicksort = do
 argv <- getArgs
 case argv of
  (x:y:z:[]) -> return $ Just ((read x :: Int, read y :: Int), read z :: Int)
  _ -> return Nothing

usage :: String -> IO ()
usage msg = do
 putStrLn msg

launch'fib'euler'sum :: String -> (Int -> Int -> Int) -> IO ()
launch'fib'euler'sum msg fn = do
 fe <- argv'fib'euler'sum
 case fe of
  (Just (f, e)) -> do
   elapsed'Show msg (fn f e `pseq` return ())
  _ -> do
   usage $ msg ++ " <fib> <euler> "

launch'fib'euler'map :: String -> ([Int] -> [Int] -> Int) -> IO ()
launch'fib'euler'map msg fn = do
 fe <- argv'fib'euler'map
 case fe of
  (Just (f, e)) -> do
   elapsed'Show msg (fn f e `pseq` return ())
  _ -> do
   usage $ msg ++ " <[fibs]> <[eulers]> "

launch'quicksort'1 :: String -> (Int -> Int -> Int -> Int) -> IO ()
launch'quicksort'1 msg fn = do
 xyz <- argv'quicksort
 case xyz of
  (Just ((x',y'),z')) -> do
   elapsed'Show msg (fn x' y' z' `pseq` return ())
  _ -> do
   usage $ msg ++ " <x> <y> <z>"

launch :: String -> a -> IO ()
launch msg fn = do
 elapsed'Show msg (fn `pseq` return ())
