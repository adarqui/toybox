-- https://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Traversable.html

import Data.Traversable
import Data.Maybe

{-
 instance Traversable Maybe where
     traverse _ Nothing = pure Nothing
     traverse f (Just x) = Just <$> f x
-}

main :: IO ()
main = do
 print "traversable"
 print $ traverse (\x -> Just 5) Nothing
 print $ traverse (\x -> Just (5+x)) $ Just 5
 print $ traverse (\x -> Just 5) [Nothing, Nothing, Nothing, Nothing]
 print $ traverse (\(Just x) -> Just (5+x)) [Just 5, Just 5, Just 5, Just 5]

 print $ for [Just 5, Just 5, Just 5, Just 5] (\(Just x) -> Just (5+x))
 print $ fmapDefault (\(Just x) -> Just (x+1)) [Just 5, Just 5]

