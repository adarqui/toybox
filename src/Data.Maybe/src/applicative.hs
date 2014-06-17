import Control.Applicative

-- https://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Applicative.html

{-
 instance Applicative Maybe where
    pure = return
    (<*>) = ap
-}

add3 a b c = a + b + c

main :: IO ()
main = do
 print "maybe applicative"
 print $ pure (+5) <*> Just 5
 print $ pure (+5) <*> Nothing
 print $ pure (+) <*> Just 5 <*> Just 5
 print $ pure (+) <*> Nothing <*> Just 5
 print $ pure add3 <*> Just 5 <*> Just 5 <*> Just 5
