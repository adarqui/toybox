{-
 instance  Functor Maybe  where
     fmap _ Nothing       = Nothing
     fmap f (Just a)      = Just (f a)
-}

main :: IO ()
main = do
 print "maybe functor"
 print $ show $ fmap (+1) Nothing
 print $ show $ fmap (+1) (Just 1)
