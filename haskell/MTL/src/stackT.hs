import StackT

import Control.Monad.State
import System.Environment (getArgs)

main :: IO ()
main = do
 argv <- getArgs
 st <- execStateT (sequence $ map push argv) []
 putStrLn $ show st
 st' <- execStateT (pop) st
 putStrLn $ show st'
 st'' <- execStateT (pop) st'
 putStrLn $ show st''
