module TB.Transformers.State.Examples (
  Person (..),
  incr,
  decr,
  toggle,
  age,
  getAge
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State.Lazy

data Person = Person {
  personAge  :: Int,
  personName :: String
} deriving (Show)

-- | Increment counter
--
-- >>> execState incr 0
-- 1
incr :: State Int ()
incr = modify (+1)

-- | Decrement counter
--
-- >>> execState decr 1
-- 0
decr :: State Int ()
decr = modify ((-)1)

-- | Toggler
--
-- >>> execState toggle False
-- True
--
-- >>> execState (toggle >> toggle) False
-- False
toggle :: State Bool ()
toggle = modify not

-- | Modify person age
--
-- >>> execState (age 32) $ Person 0 "adarqui"
-- Person {personAge = 32, personName = "adarqui"}
age :: Int -> State Person ()
age n = do
  modify' (\p -> p { personAge = n })
  return ()

-- | Get person age
--
-- >>> evalState getAge $ Person 32 "adarqui"
-- 32
getAge :: State Person Int
getAge = gets personAge

-- | Functor
--
-- >>> fmap (+1) $ runState incr 0
-- ((),2)
--
-- >>> snd . fmap not $ runState toggle False
-- False

-- | Applicative
--
-- >>> snd $ (+1) <$> runState incr 0
-- 2
--
-- >>> snd $ pure (+1) <*> runState incr 0
-- 2

-- | Alternative
--

-- | Monad
--

-- | MonadPlus
--

-- | MonadFix
--
