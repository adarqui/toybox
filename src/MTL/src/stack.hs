import Stack

import Control.Monad.State
import System.Environment (getArgs)

main :: IO ()
main = do
 argv <- getArgs
 let st = execState (sequence $ map push argv) []
 putStrLn $ show st
 let st' = execState (pop) st
 putStrLn $ show st'
 let st'' = execState (pop) st'
 putStrLn $ show st''
