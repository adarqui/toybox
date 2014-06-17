import Data.Either

{-
 data  Either a b  =  Left a | Right b
   deriving (Eq, Ord, Read, Show, Typeable)
 instance Functor (Either a) where
     fmap _ (Left x) = Left x
     fmap f (Right y) = Right (f y)
-}

main :: IO ()
main = do
 print "functor"
 print $ fmap (++"!") (Left "err")
 print $ fmap (\s -> s++"!") (Right "value")
