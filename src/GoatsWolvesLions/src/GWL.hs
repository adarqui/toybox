module Main (
 Forest(..),
 wolfDevoursGoat,
 lionDevoursGoat,
 lionDevoursWolf,
 gwlMeal,
 isStable,
 findStableForests,
 main
) where

import System.Environment
import Data.Maybe
import Data.List

data Forest = Forest { goats, wolves, lions :: !Int } deriving (Show, Eq, Ord)
tf = True || False

wolfDevoursGoat :: Forest -> Maybe Forest
wolfDevoursGoat (Forest g w l)
 | g > 0 && w > 0 = Just (Forest {goats=g-1,wolves=w-1,lions=l+1})
 | tf = Nothing

lionDevoursGoat :: Forest -> Maybe Forest
lionDevoursGoat (Forest g w l)
 | g > 0 && l > 0 = Just (Forest {goats=g-1,wolves=w+1,lions=l-1})
 | tf = Nothing

lionDevoursWolf :: Forest -> Maybe Forest
lionDevoursWolf (Forest g w l)
 | w > 0 && l > 0 = Just (Forest {goats=g+1,wolves=w-1,lions=l-1})
 | tf = Nothing

gwlMeal gwl@(Forest g w l) = catMaybes [wolfDevoursGoat gwl, lionDevoursGoat gwl, lionDevoursWolf gwl]

isStable gwl@(Forest goats wolves lions)
 | goats == 0 = wolves == 0 || lions == 0
 | tf = wolves == 0 && lions == 0

forestCmp forest1@(Forest g1 w1 l1) forest2@(Forest g2 w2 l2) =
 case cmpg of
  EQ -> case cmpw of
       EQ -> compare l1 l2
       _ -> cmpw
  _ -> cmpg
  where
   cmpg = compare g1 g2
   cmpw = compare w1 w2

meal :: [Forest] -> [Forest]
meal [] = []
meal forests = nub $ foldr (\y x-> (gwlMeal y) ++ x) [] forests
--meal forests = nub $ sortBy forestCmp $ foldr (\y x-> (gwlMeal y) ++ x) [] forests
--meal forests = nub $ sortBy forestCmp $ foldl' (\x y-> (gwlMeal y) ++ x) [] forests

devouringPossible [] = False
devouringPossible forests = not $ any isStable forests

stableForests :: [Forest] -> [Forest]
stableForests forests = filter isStable forests

findStableForests :: Forest -> [Forest]
findStableForests forest@(Forest g w l) = stableForests $ findStableForests' [forest]
 where
  findStableForests' [] = []
  findStableForests' forests =
   case (devouringPossible forests) of
    True -> findStableForests' $ meal forests
    _ -> forests


main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (g:w:l:[]) -> putStrLn $ show $ findStableForests (Forest {goats = ri g, wolves= ri w, lions = ri l})
  _ -> usage
 where
  usage = putStrLn "./gwl goats wolves lions"
  ri i = read i :: Int
