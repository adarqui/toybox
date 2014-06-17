import Data.Maybe
import Data.List
import Control.Monad

{-
 data  Maybe a  =  Nothing | Just a
    deriving (Eq, Ord)
-}

t1 = do
 case Nothing of
  Nothing -> do
   case (Just 5) of
    Nothing -> print "Nothing"
    (Just v) -> print v
  _ -> print "Something"

{-
 maybe :: b -> (a -> b) -> Maybe a -> b
 maybe n _ Nothing  = n
 maybe _ f (Just x) = f x
-}

t2 = do
 print $ maybe 100 (+1) Nothing
 print $ maybe 100 (+1) (Just 0)

t3 = do
 case (isJust $ Just 5) of
  True -> print "true"
  False -> print "False"

{-
 listToMaybe           :: [a] -> Maybe a
 listToMaybe []        =  Nothing
 listToMaybe (a:_)     =  Just a
-}

t4 = do
 print $ show $ listToMaybe [1,2,3,4,5]

{-
 catMaybes              :: [Maybe a] -> [a]
 catMaybes ls = [x | Just x <- ls]
-}

t5 = do
 print $ show $ catMaybes [Just 1, Just 2, Just 3]

{-
 maybeToList            :: Maybe a -> [a]
 maybeToList  Nothing   = []
 maybeToList  (Just x)  = [x]
-}

t6 = do
 print $ show $ maybeToList (Just 1)

{-
 mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
 mapMaybe _ []     = []
 mapMaybe f (x:xs) =
  let rs = mapMaybe f xs in
  case f x of
   Nothing -> rs
   Just r  -> r:rs
-}

t7 = do
 print $ show $ mapMaybe (\x -> x) [Just 1, Nothing, Just 3, Nothing, Just 5]

{-
 fromMaybe     :: a -> Maybe a -> a
 fromMaybe d x = case x of {Nothing -> d;Just v  -> v}
-}

t8 = do
 print $ fromMaybe 5 Nothing

main :: IO ()
main = do
 print "maybe"
 foldM_ (\x y -> banner x >> y >> return (x+1)) 1 [t1, t2, t3, t4, t5, t6, t7, t8]
 return ()
 where
  banner n = do
   print $ "--------------------: t" ++ show n
