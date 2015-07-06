import Criterion.Measurement
 (runForAtLeast)

f :: Int -> IO Int
f n = do
 _ <- return [1..n]
 putStrLn $ "n = " ++ show n
 return $ n+1

main :: IO ()
main = do
 tup <- runForAtLeast 0.2 0 f
 print $ show tup
 return ()
