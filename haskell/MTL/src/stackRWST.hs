import StackRWST

import Control.Monad.RWS
import System.Environment (getArgs)

type X = RWST Int [Int] [String] IO [String]

runIt :: X -> IO [String]
runIt p = do
 (a, st, w) <- runRWST p 0 []
 return st

main :: IO ()
main = do
 argv <- getArgs
 print "rws"
 st <- runIt $ do
  push "hi"
  push "yo"
  push "test"
  pop
  contents
 putStrLn $ show st
 return ()
