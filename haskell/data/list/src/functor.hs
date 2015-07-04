{-
instance Functor [] where
   fmap = map
-}

main :: IO ()
main = do
 print "functor"
 print $ fmap (+1) [1,2,3,4]
