import Control.Applicative
import Data.Maybe

-- https://hackage.haskell.org/package/base-4.7.0.0/docs/src/Control-Applicative.html

{-
 instance Alternative Maybe where
     empty = Nothing
     Nothing <|> r = r
     l       <|> _ = l
-}

add3 a b c = a + b + c

main :: IO ()
main = do
 print $ Just 5 <|> empty
 print $ Nothing <|> Just 5
 print $ Just 5 <|> Nothing
 print $ isNothing $ Nothing <|> Nothing
 print $ Nothing <|> Just 5 <|> Just 6 <|> Just 7
