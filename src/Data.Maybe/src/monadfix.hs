-- http://www.haskell.org/ghc/docs/7.6.3/html/libraries/base/src/Control-Monad-Fix.html

import Control.Monad.Fix

{-
 instance MonadFix Maybe where
     mfix f = let a = f (unJust a) in a
              where unJust (Just x) = x
                    unJust Nothing  = error "mfix Maybe: Nothing"
-}

main :: IO ()
main = do
 print "monadfix"
 print $ mfix (\x -> Just 5)
