import Control.Applicative

-- https://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Applicative.html

{-
 instance Alternative [] where
     empty = []
     (<|>) = (++)
-}

main :: IO ()
main = do
 print "alternative"
 print $ [1,2,3] <|> [4,5,6]
 print $ [1,2,3] <|> empty
