-- http://research.microsoft.com/~simonpj/papers/stm/Santa.hs.gz

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import System.Random

import Prelude hiding (id)


data Group = MkGroup Int (TVar (Int, Gate, Gate))
data Gate  = MkGate Int (TVar Int)


helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do
 (in_gate, out_gate) <- joinGroup group
 passGate in_gate
 do_task
 passGate out_gate


elf1, reindeer1 :: Group -> Int -> IO ()
reindeer1 group id = helper1 group (deliverToys id)
elf1      group id = helper1 group (meetInStudy id)


meetInStudy, deliverToys :: Int -> IO ()
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study\n")
deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys\n")


newGroup :: Int -> IO Group
newGroup n = do
 atomically $ do
  g1 <- newGate n
  g2 <- newGate n
  tv <- newTVar (n, g1, g2)
  return (MkGroup n tv)


joinGroup :: Group -> IO (Gate,Gate)
joinGroup (MkGroup _ tv) = do
 atomically $ do
  (n_left, g1, g2) <- readTVar tv
  check (n_left > 0)
  writeTVar tv (n_left-1, g1, g2)
  return (g1,g2)


awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) = do
 (n_left, g1, g2) <- readTVar tv
 check (n_left == 0)
 new_g1 <- newGate n
 new_g2 <- newGate n
 writeTVar tv (n,new_g1,new_g2)
 return (g1,g2)


newGate :: Int -> STM Gate
newGate n = do
 tv <- newTVar 0
 return (MkGate n tv)


passGate :: Gate -> IO ()
passGate (MkGate _ tv) = do
 atomically $ do
  n_left <- readTVar tv
  check (n_left > 0)
  writeTVar tv (n_left-1)


operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
 atomically (writeTVar tv n)
 atomically $ do
  n_left <- readTVar tv
  check (n_left == 0)


forever :: IO () -> IO ()
forever act = do
 act
 forever act


randomDelay :: IO ()
randomDelay = do
 waitTime <- getStdRandom (randomR (1, 1000000))
 threadDelay waitTime


choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do
 to_do <- atomically (foldr1 orElse stm_actions)
 to_do
 where
  stm_actions :: [STM (IO ())]
  stm_actions = [
    do
     val <- guard
     return (rhs val)
    | (guard, rhs) <- choices
   ]


santa :: Group -> Group -> IO ()
santa elf_group rein_group = do
 putStr "----------\n"
 choose
  [(awaitGroup rein_group, run "deliver toys"),
   (awaitGroup elf_group,  run "meet in the study")]
 where
  run :: String -> (Gate,Gate) -> IO ()
  run task (in_gate,out_gate) = do
   putStr ("Ho! Ho! Ho! let's " ++ task ++ "\n")
   operateGate in_gate
   operateGate out_gate


main :: IO ()
main = do
 elf_gp <- newGroup 3
 rein_gp <- newGroup 9
 sequence_ [ run elf1 elf_gp n | n <- [1..10]]
 sequence_ [ run reindeer1 rein_gp n | n <- [1..9]]
 forever (santa elf_gp rein_gp)
 where
  run f gp id = forkIO $ do
   forever $ do
   _ <- f gp id
   randomDelay
