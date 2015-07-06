module TB.Servant.API.Counter (
  CounterAPI,
  counterAPI,
  server,
  runServer
) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified TB.STM.Counter as C
import           TB.STM.Counter (Counter)

type Store = TVar Counter

instance FromJSON Counter
instance ToJSON Counter

type CounterAPI =
        "counter_tvar" :> Get '[JSON] Counter
  :<|>  "counter_tvar" :> Post '[JSON] Counter
  :<|>  "counter_tvar" :> Delete '[JSON] Counter

counterAPI :: Proxy CounterAPI
counterAPI = Proxy

server :: Store -> Server CounterAPI
server store = getCounterH :<|> postCounterH :<|> deleteCounterH
  where
    getCounterH :: MonadIO m => m Counter
    getCounterH = liftIO $ getCounter store

    postCounterH :: MonadIO m => m Counter
    postCounterH = liftIO $ postCounter store

    deleteCounterH :: MonadIO m => m Counter
    deleteCounterH = liftIO $ deleteCounter store

getCounter :: Store -> IO Counter
getCounter store = do
  C.get store

postCounter :: Store -> IO Counter
postCounter store = do
  C.incr store

deleteCounter :: Store -> IO Counter
deleteCounter store = do
  C.reset store

app :: Store -> Application
app store = serve counterAPI $ server store

runServer :: IO ()
runServer = do
  store <- C.new
  run 8080 $ app store
