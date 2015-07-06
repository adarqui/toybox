module TB.Pipes.Examples (
) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Pipes
import qualified Pipes.Prelude       as P
import           System.IO

stdinLn' :: Producer String IO ()
stdinLn' = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn'

stdinOneLn :: Producer String IO ()
stdinOneLn = do
  str <- lift getLine
  yield str

stdoutLn' :: Consumer String IO ()
stdoutLn' = do
  str <- await
  lift $ putStrLn str
  stdoutLn'

fileLn :: String -> Consumer String IO ()
fileLn file = go
  where
    go = do
      str <- await
      lift $ appendFile file (str ++ "\n")
      go

revGetLine :: IO String
revGetLine = liftM reverse getLine

revInput :: Monad m => Proxy () [a] y' y m [a]
revInput = liftM reverse await

pipe01 :: IO ()
pipe01 = runEffect $ for P.stdinLn (lift . putStrLn)

pipe02 :: IO ()
pipe02 = runEffect $ P.stdinLn >-> P.stdoutLn

pipe03 :: IO ()
pipe03 = runEffect $ lift getLine >~ P.stdoutLn

pipe04 :: IO ()
pipe04 = runEffect $ P.stdinLn >-> P.takeWhile (/= "quit") >-> P.stdoutLn

pipe05 :: IO ()
pipe05 = runEffect $ P.stdinLn >-> P.tee P.stdoutLn >-> P.tee P.stdoutLn >-> P.stdoutLn

pipe06 :: IO ()
pipe06 = runEffect $ stdinOneLn >-> P.stdoutLn

pipe07 :: IO ()
pipe07 = runEffect $ P.repeatM getLine >-> P.stdoutLn

pipe08 :: IO ()
pipe08 = runEffect $ P.replicateM 3 getLine >-> P.stdoutLn

pipe09 :: IO ()
pipe09 = runEffect $ lift getLine >~ P.stdoutLn

pipe10 :: IO ()
pipe10 = runEffect $ P.stdoutLn <-< P.stdinLn

pipe11 :: IO ()
pipe11 = runEffect $ P.stdinLn >-> P.tee (fileLn "/tmp/log.txt") >-> P.stdoutLn

mapP :: Monad m => (a -> a) -> [a] -> Producer a m ()
mapP f xs = Control.Monad.mapM_ (yield . f) xs

mapP' :: (Monad m, Foldable f) => (a -> a) -> f a -> Proxy x' x () a m ()
mapP' f xs = foldMap (yield . f) xs

pipe12 :: IO ()
pipe12 = runEffect $ for (mapP (+1) [1,2,3]) (lift . putStrLn . show)

pipe13 :: IO ()
pipe13 = runEffect $ yield "hello" >-> P.stdoutLn

pipe14 :: IO ()
pipe14 = runEffect $ for (mapP' (+1) (1,2)) (lift . putStrLn . show)

pipe15 :: IO ()
pipe15 = runEffect $ for (mapP' (+1) (Just 1)) (lift . putStrLn . show)

pipe16 :: IO ()
pipe16 = runEffect $ lift getLine >~ lift getLine >~ lift revGetLine >~ P.stdoutLn

pipe17 :: IO ()
pipe17 = runEffect $ lift getLine >~ revInput >~ P.stdoutLn

{-
- P.stdinLn :: MonadIO m => Proxy x' x () String m ()
- P.stdoutLn :: MonadIO m => Proxy () String y' y m ()
- (>->) :: Monad m => Proxy a' a () b m r -> Proxy () b c' c m r -> Proxy a' a c' c m r
- (<-<) :: Monad m => Proxy () b c' c m r -> Proxy a' a () b m r -> Proxy a' a c' c m r
- (<~)  :: Monad m => (b -> Proxy x' x c' c m b') -> (a -> Proxy x' x b' b m a') -> a -> Proxy x' x c' c m a'
- for   :: Monad m => Proxy x' x b' b m a' -> (b -> Proxy x' x c' c m b') -> Proxy x' x c' c m a'
- next  :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))
- (~>)  :: Monad m => (a -> Proxy x' x b' b m a') -> (b -> Proxy x' x c' c m b') -> a -> Proxy x' x c' c m a'
- await :: Monad m => Proxy () a y' y m a
- yield :: Monad m => a -> Proxy x' x () a m ()
- drain :: Monad m => Proxy () a y' y m r
-
- laws:
- for (for s f) g = for s (\\x -> for (f x) g)
- (f ~> g) x = for (f x) g
- (f ~> g) ~> h = f ~> (g ~> h)
- yield ~> f = f
- f ~> yield = f
- for (yield x) f = f x
- for s yield = s
- (f >~ g) >~ h = f >~ (g >~ h)
- f >~ g >~ h
- await >~ f = f
- f >~ await = f

- yield: Send output data
- await: Receive input data
- Producer's can only 'yield' values and they model streaming sources
- Consumer's can only 'await' values and they model streaming sinks
- Pipe's can both 'yield' and 'await' values and they model stream transformations
- Effect's can neither 'yield' nor 'await' and they model non-streaming components
- for: handles 'yield's
- (>~) handles 'await's
- (>->) handles both 'yield's and 'await's
- (>>=) handles return values
-}
