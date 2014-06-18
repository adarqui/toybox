sq :: Num n => n -> n
sq n = n * n

sq' :: Num n -> n -> n
sq' d n = (*) d n n

main :: IO ()
main = do
 print $ sq 8
