import Control.Monad

{-
 instance  Monad Maybe  where
     (Just x) >>= k      = k x
     Nothing  >>= _      = Nothing
     (Just _) >>  k      = k
     Nothing  >>  _      = Nothing
     return              = Just
     fail _              = Nothing
-}

m1 :: Monad m => m (Maybe a)
m1 = do
 return Nothing

m2 :: (Num a, Monad m) => m (Maybe a)
m2 = do
 return $ Just 5

m3 :: (Num a, Monad m) => m (Maybe a)
m3 = do
 return Nothing >> return Nothing >> return $ Just 10

m4 :: (Num a, Monad m) => m (Maybe a)
m4 = do
 fail "failure"

m5 :: (Num a, Monad m) => m (Maybe a)
m5 = do
-- (Just 7) >>= \y -> return y
 return $ Just 7

-- ^^ extra monad context

{-
 instance  Monad Maybe  where
     (Just x) >>= k      = k x
     Nothing  >>= _      = Nothing
     (Just _) >>  k      = k
     Nothing  >>  _      = Nothing
     return              = Just
     fail _              = Nothing
-}

m1' :: Maybe a
m1' = do
 Nothing

m2' :: (Num a) => Maybe a
m2' = do
 Just 5

m3' :: (Num a) => Maybe a
m3' = do
 Nothing >> Nothing >> Just 10

m4' :: Maybe a
m4' = do
 fail "failure"

m5' :: (Num a) => Maybe a
m5' = do
 (Just 7) >>= return

main :: IO ()
main = do
 print "maybe monad"
 m <- mapM (\x -> x >>= return) [m1, m2, m3]
 mapM_ (\x -> case x of {Nothing -> print "Nothing"; (Just v) -> print v}) m
 foldM_ (\x y -> banner x >> (print y) >> return (x+1)) 1 [m1',m2',m3',m4',m5']
 return ()
 where
  banner n = do
   print $ "--------------------: t" ++ show n
