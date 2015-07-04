import Control.Applicative

-- https://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Applicative.html

{-
 instance Applicative [] where
     pure = return
     (<*>) = ap
-}

main :: IO ()
main = do
 print "applicative"
 print $ (+) <$> [1,2,3] <*> [4,5,6]
 print $ (++) <$> ["a","b","c"] <*> ["1","2","3"]
