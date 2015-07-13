module TB.System.Random.List (
  shuffleList,
  shuffleListCore,
  shuffleListBase
) where

import           Data.Function
import           Data.List
import           System.Random

shuffleList :: Int -> [a] -> [a]
shuffleList seed xs = map snd $ sortBy (compare `on` fst) $ zip rands xs
  where
    rands = randoms (mkStdGen seed) :: [Int]

shuffleListCore :: Ord b => [(b, a)] -> [a]
shuffleListCore xs = map snd $ sortBy (compare `on` fst) xs

shuffleListBase :: Int -> [a] -> [a]
shuffleListBase seed xs = shuffleListCore $ zip rands xs
  where
    rands = randoms (mkStdGen seed) :: [Int]
