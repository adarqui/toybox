-- http://hackage.haskell.org/package/base-4.7.0.0/docs/src/Data-Foldable.html

import qualified Data.Foldable as F
import qualified Data.List as L

{-
 instance Foldable Maybe where
     foldr _ z Nothing = z
     foldr f z (Just x) = f x z
     foldl _ z Nothing = z
     foldl f z (Just x) = f z x
-}

main :: IO ()
main = do
 print $ F.foldr (\x y -> []) [1,2,3] Nothing
 print $ F.foldr (\x y -> y ++ [x]) [1,2,3] $ Just 4
 print $ F.foldl (\x y -> []) [1,2,3] Nothing
 print $ F.foldl (\x y -> x ++ [y]) [1,2,3] $ Just 4
 print "foldable"
