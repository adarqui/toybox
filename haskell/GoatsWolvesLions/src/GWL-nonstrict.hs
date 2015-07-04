module Main (
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

type Forest' = (Integer, Integer, Integer)
tf = True || False

wolfDevoursGoat :: Forest' -> Maybe Forest'
wolfDevoursGoat (goats,wolves,lions)
 | goats > 0 && wolves > 0 = Just (goats-1,wolves-1,lions+1)
 | tf = Nothing

lionDevoursGoat :: Forest' -> Maybe Forest'
lionDevoursGoat (goats,wolves,lions)
 | goats > 0 && lions > 0 = Just (goats-1,wolves+1,lions-1)
 | tf = Nothing

lionDevoursWolf :: Forest' -> Maybe Forest'
lionDevoursWolf (goats,wolves,lions)
 | wolves > 0 && lions > 0 = Just (goats+1,wolves-1,lions-1)
 | tf = Nothing

gwlMeal gwl@(goats,wolves,lions) = catMaybes [wolfDevoursGoat gwl, lionDevoursGoat gwl, lionDevoursWolf gwl]

isStable gwl@(goats,wolves,lions)
 | goats == 0 = wolves == 0 || lions == 0
 | tf = wolves == 0 && lions == 0

forestCmp forest1@(g1,w1,l1) forest2@(g2,w2,l2) =
 case cmpg of
  EQ -> case cmpw of
       EQ -> compare l1 l2
       _ -> cmpw
  _ -> cmpg
  where
   cmpg = compare g1 g2
   cmpw = compare w1 w2

meal :: [Forest'] -> [Forest']
meal [] = []
meal forests = nub $ sortBy forestCmp $ foldr (\y x-> (gwlMeal y) ++ x) [] forests

devouringPossible [] = False
devouringPossible forests = not $ any isStable forests

stableForests :: [Forest'] -> [Forest']
stableForests forests = filter isStable forests

findStableForests :: Forest' -> [Forest']
findStableForests forest@(g,w,l) = stableForests $ findStableForests' [forest]
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
  (goats:wolves:lions:[]) -> putStrLn $ show $ findStableForests (ri goats, ri wolves, ri lions)
  _ -> usage
 where
  usage = putStrLn "./gwl goats wolves lions"
  ri i = read i :: Integer
