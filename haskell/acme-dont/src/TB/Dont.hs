module TB.Dont (
  don't'
) where

import           Acme.Dont (don't)

-- | Ignores the supplied action.
--
-- Examples:
--
-- >>> don't $ return "test"
--
--
-- >>> don't' $ return "test"
--
don't' :: Monad m => m a -> m ()
don't' _ = return ()
