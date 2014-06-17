{-
 instance  Monad []  where
     m >>= k             = foldr ((++) . k) [] m
     m >> k              = foldr ((++) . (\ _ -> k)) [] m
     return x            = [x]
     fail _              = []
-}

import Control.Monad

m1 :: [a]
m1 = []

m2 :: [a]
m2 = fail "test"

m3 :: [a]
m3 = [1,2,3] >> []

m4 :: (Num a) => [a]
m4 = [1,2,3] >> [1]

m5 :: (Num a) => [a]
m5 = fail "failure"

m6 :: (Num a) => [a]
m6 = [1,2,3] >> [4,5,6] >> [8,8,8]

-- foldr ((++) . (\x -> [x+1])) [] [1,2,3]
m7 :: (Num a) => [a]
m7 = [1,2,3] >>= (\x -> [x+1]) >>= (\x -> [2*x])

main :: IO ()
main = do
 print "monad"
 foldM_ (\x y -> banner x >> (print y) >> return (x+1)) 1 [m1,m2,m3,m4,m5,m6,m7]
 return ()
 where
  banner n = do
   print $ "--------------------: t" ++ show n
