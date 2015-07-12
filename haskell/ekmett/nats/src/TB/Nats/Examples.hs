module TB.Nats.Examples (
  fact
) where

import           Numeric.Natural
import Control.Exception
import Control.Monad

fact :: Natural -> Natural
fact 0 = 1
fact n = n * fact (n - 1)

errorFn :: Natural -> Natural
errorFn n = n - 1

errorIO :: Natural -> IO ()
errorIO n = do
  catch
    (putStrLn (show $ errorFn n))
    arithExceptionHandler

arithExceptionHandler :: ArithException -> IO ()
arithExceptionHandler _ = putStrLn "arithmetic exception"
