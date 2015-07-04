import Control.Monad.State

counter :: StateT Int IO Int
counter = do
 liftIO $ putStrLn "counter"
 n <- get
 put (n+1)
 return n

counter' :: StateT Int IO ()
counter' = do
 modify (\s -> s+1)

getCounter :: StateT Int IO Int
getCounter = get >>= return

setState :: Int -> StateT Int IO Int
setState n = do
 put n
 get >>= return

main :: IO ()
main = do
 st <- execStateT (sequence $ replicate 100 counter) 0
 putStrLn $ show st
 return ()
