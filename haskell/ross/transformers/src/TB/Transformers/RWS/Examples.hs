module TB.Transformers.RWS.Examples (
  incr,
  decr,
  logm,
  example01
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.RWS.Lazy

incr :: RWS Bool [String] Int ()
incr = modify (+1)

decr :: RWS Bool [String] Int ()
decr = modify ((-)1)

logm :: String -> RWS Bool [String] Int ()
logm m = tell [m]

-- | Example
--
-- >>> runRWS example01 True 0
-- ((),4,["2","4"])
example01 = do
  incr
  incr
  counter <- get
  logm (show counter)
  incr
  incr
  counter' <- get
  logm (show counter')
