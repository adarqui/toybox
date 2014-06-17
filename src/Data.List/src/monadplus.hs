import Control.Monad

-- http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Control-Monad.html

{-
 instance MonadPlus [] where
    mzero = []
    mplus = (++)
-}

main :: IO ()
main = do
 print "monadplus"
 print $ "hell" `mplus` "o" `mplus` "!" `mplus` mzero
