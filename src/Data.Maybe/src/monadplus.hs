import Control.Monad
import Data.Maybe

-- http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Control-Monad.html

{-
 instance MonadPlus Maybe where
    mzero = Nothing
    Nothing `mplus` ys  = ys
    xs      `mplus` _ys = xs
-}

m1 = do
 Nothing `mplus` Just 5

m2 = do
 mzero `mplus` Just 6

m3 = do
 Just 7 `mplus` Nothing

m4 = do
 Nothing `mplus` Nothing

m5 = do
 Just 8 `mplus` Just 9

main :: IO ()
main = do
 print "monadplus"
 print $ m1
 print $ m2
 print $ m3
 print $ isNothing m4
 print $ m5
