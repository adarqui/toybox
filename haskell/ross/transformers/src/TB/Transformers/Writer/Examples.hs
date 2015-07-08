module TB.Transformers.Writer.Examples (
  incr,
  decr,
  logm
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Writer.Lazy
import           Data.Monoid

instance Monoid Int where
  mempty = 0
  x `mappend` y = x + y

-- | Increment the counter
--
-- >>> execWriter incr
-- 1
incr :: Writer Int ()
incr = tell 1

-- | Decrement the counter
--
-- >>> execWriter (incr >> decr)
-- 0
decr :: Writer Int ()
decr = tell (-1)

-- | Logger
--
-- >>> execWriter (logm "log" >> logm "message")
-- ["log","message"]
logm :: String -> Writer [String] ()
logm msg = tell [msg]

-- | censor
--
-- >>> execWriter (censor reverse $ logm "hey" >> logm "yo")
-- ["yo","hey"]

-- | listen
--
-- >>> runWriter (incr >> listen incr)
-- (((),1),2)

-- | listens
--
-- >>> runWriter (incr >> listens (*(-1)) (incr >> incr))
-- (((),-2),3)
