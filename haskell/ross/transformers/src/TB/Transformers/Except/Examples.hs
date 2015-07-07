module TB.Transformers.Except.Examples (
  safeDiv,
  safeDiv',
  runSafeDiv'
) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Traversable

-- | safe division
--
-- >>> safeDiv 4 2
-- Right 2
--
-- >>> safeDiv 4 0
-- Left "division by zero"
safeDiv :: (Num a, Integral a) => a -> a -> Either String a
safeDiv x y =
  runExcept $ do
    case y of
      0 -> throwE "division by zero"
      _ -> return (x `div` y)

-- | safe division
--
-- >>> runExcept (safeDiv' 4 2)
-- Right 2
--
-- >>> runExcept (safeDiv' 4 0)
-- Left "division by zero"
safeDiv' :: (Num a, Integral a) => a -> a -> Except String a
safeDiv' _ 0 = throwE "division by zero"
safeDiv' x y = return (x `div` y)

runSafeDiv' x y = runExcept (safeDiv' x y)

-- | except
t_except = runIdentity $ runExceptT $ except $ safeDiv 0 0

-- | mapExcept
t_mapExcept :: (Num a, Integral a) => Either String a
t_mapExcept = runExcept (mapExcept (\r -> either (Left . id) (Right . (+1)) r) (safeDiv' 4 2))

-- | withExcept
t_withExcept :: (Num a, Integral a) => Except String a
t_withExcept = withExcept id (safeDiv' 4 2)

-- | catchE
t_catchE :: (Num a, Integral a) => ExceptT Bool IO a
t_catchE = safeDivIO' 4 0 `catchE` (\_ -> throwE False)

t_catchE' :: (Num a, Integral a) => IO (Either Bool a)
t_catchE' = runExceptT t_catchE

-- some io variations

safeDivIO :: (Num a, Integral a) => a -> a -> IO (Either String a)
safeDivIO x y =
  runExceptT $ do
    case y of
      0 -> (liftIO $ putStrLn "division by zero") >> throwE "division by zero"
      _ -> return (x `div` y)

safeDivIO' :: (Num a, Integral a) => a -> a -> ExceptT String IO a
safeDivIO' _ 0 = do
  liftIO $ putStrLn "division by zero"
  throwE "division by zero"
safeDivIO' x y = return (x `div` y)

-- | other
--
-- >>> runExcept (throwE "error" >> return True)
-- Left "error"
--
-- >>> runExcept (return True >> throwE "error" >> return False)
-- Left "error"
--
-- >>> fmap (+1) $ runExcept (return 1)
-- Right 2
--
-- >>> (+1) <$> runExcept (return 1)
-- Right 2
--
-- >>> foldMap (fmap (+1)) $ runExcept (return [1,2,3])
-- [2,3,4]
--
-- >>> traverse (fmap (+1)) $ runExcept (return [1,2,3])
-- [Right 2,Right 3,Right 4]
--
-- >>> :{
--  do
--    x <- runExcept (return 1)
--    y <- runExcept (return 1)
--    return (x + y)
-- :}
-- Right 2
