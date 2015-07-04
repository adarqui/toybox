import System.Environment
import AFP08
import Fib

runFib :: Int -> Int -> IO ()
runFib idx num = do
 case idx of
  1 -> launch "fib1" (fib1 num)
  _ -> putStrLn "invalid index"

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (idx:num:[]) -> do
   runFib (read idx :: Int) (read num :: Int)
  _ -> putStrLn "usage: ./fib <index> <num>"
