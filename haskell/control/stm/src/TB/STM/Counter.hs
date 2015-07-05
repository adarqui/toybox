{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}

module TB.STM.Counter (
  Counter,
  new,
  reset,
  incr,
  decr,
  set,
  get
) where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar,
                                         newTVarIO, readTVar, readTVarIO)
import           Data.Monoid            (Monoid, mappend, mempty)
import           GHC.Generics

newtype Counter = Counter Int deriving (Eq, Enum, Show, Generic)

instance Monoid Counter where
  mempty = Counter 0
  Counter a `mappend` Counter b = Counter (a+b)

-- $setup
-- >>> tv <- new

-- | Creates a new transactional Counter
--
-- >>> _tv <- new
--
new :: IO (TVar Counter)
new = do
  newTVarIO $ (mempty :: Counter)

-- | Reset a transactional counter to zero
--
-- >>> reset tv
-- Counter 0
reset :: TVar Counter -> IO Counter
reset = set 0

-- | Increments our transactional counter by 1
--
-- >>> incr tv
-- Counter 1
incr :: TVar Counter -> IO Counter
incr = abstracted succ

-- | Decrements our transactional counter by 1
--
-- >>> decr tv
-- Counter (-1)
decr :: TVar Counter -> IO Counter
decr = abstracted pred

-- | Set our transactional counter to a specific value
--
-- >>> set 5 tv
-- Counter 5
set :: Int -> TVar Counter -> IO Counter
set n = abstracted (const (Counter n))

-- | Get the value of our transactional counter
--
-- >>> get tv
-- Counter 0
get :: TVar Counter -> IO Counter
get = readTVarIO

-- | Abstracted atomically function.
abstracted :: (Counter -> Counter) -> TVar Counter -> IO Counter
abstracted op tv = do
  atomically $ do
    modifyTVar tv op
    readTVar tv
