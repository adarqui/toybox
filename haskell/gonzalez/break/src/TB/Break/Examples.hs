module TB.Break.Examples (
  fact
) where

import           Control.Break
import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Numeric.Natural
import           Prelude                   hiding (break)

-- | factorial
--
-- >>> fact 0
-- 1
--
-- >>> fact 1
-- 1
--
-- >>> fact 5
-- 120
fact :: Natural -> Natural
fact n = fst $ execState fact' (1, n)

fact' :: State (Natural, Natural) Natural
fact' = loop $ do
  (acc, n) <- lift get
  case n of
    0 -> break acc
    _ -> lift $ put (acc*n, n-1)


-- | uselessness
--
-- >>> useless
-- 1
useless :: Natural
useless = runIdentity useless'

useless' :: Identity Natural
useless' = loop $ do
  break 1
