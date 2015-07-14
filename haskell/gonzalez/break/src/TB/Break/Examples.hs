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

fact' :: StateT (Natural, Natural) Identity (Natural, Natural)
fact' = loop $ do
  (acc, n) <- lift get
  case n of
    0 -> break (1, n)
    _ -> lift $ put (acc*n, n-1)


useless :: Natural -> Natural
useless = runIdentity . useless'

useless' :: Natural -> Identity Natural
useless' n = loop $ do
  break 1
