module TB.Transformers.Error.Examples (
) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Error
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Traversable

-- ***Use Except instead of Error***

-- | safe division
--
-- >>> safeDivIO 4 2
-- Right 2
--
-- >>> safeDivIO 4 0
-- division by zero
-- Left "division by zero"
safeDivIO :: (Num a, Integral a) => a -> a -> IO (Either String a)
safeDivIO x y =
  runErrorT $ do
    case y of
      0 -> (liftIO $ putStrLn "division by zero") >> throwError "division by zero"
      _ -> return (x `div` y)

safeDivIO' :: (Num a, Integral a) => a -> a -> ErrorT String IO a
safeDivIO' _ 0 = do
  liftIO $ putStrLn "division by zero"
  throwError "division by zero"
safeDivIO' x y = return (x `div` y)
