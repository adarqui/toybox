import StackRWST

import Control.Monad.Trans.RWS
{-
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
-}
import System.Environment (getArgs)

main :: IO ()
main = do
 argv <- getArgs
 print "rws"
 (st, w) <- execRWST (pop) [] []
-- putStrLn $ show st
-- st' <- execRWST (pop) [] st
 return ()
 {-
 putStrLn $ show st'
 st'' <- execStateT (pop) st'
 putStrLn $ show st''-}
