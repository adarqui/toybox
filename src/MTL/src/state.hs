import Control.Monad.State

counter :: State Int Int
counter = do
 n <- get
 put (n+1)
 return n

counter' :: State Int ()
counter' = do
 modify (\s -> s+1)

getCounter :: State Int Int
getCounter = get >>= return

setState :: Int -> State Int Int
setState n = do
 put n
 get >>= return

main :: IO ()
main = do
 let st = execState (sequence $ replicate 100 counter) 0
 putStrLn $ show st
 let st' = snd $ runState ((sequence $ replicate 100 counter') >> getCounter) 0
 putStrLn $ show $ st'
 let st'' = snd $ runState ((sequence $ replicate 100 counter') >> getCounter >> setState 99) 0
 putStrLn $ show $ st''
 return ()
