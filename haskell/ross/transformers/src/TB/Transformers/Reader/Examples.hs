module TB.Transformers.Reader.Examples (
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader

-- | ask
--
-- >>> runReader (return 1) 5
-- 1
--
-- >>> runReader ask 5
-- 5

-- | asks
--
-- >>> runReader (asks (+1)) 5
-- 6

-- | local
--
-- >>> runReader (local (+100) ask) 5
-- 105
