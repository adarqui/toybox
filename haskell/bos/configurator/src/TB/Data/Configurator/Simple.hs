{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TB.Data.Configurator.Simple (
  withConfig,
  withConfigReload,
  defaultWorthFiles,
  tmpWorthFiles,
  printCb,
  printCbForever,
  example,
  exampleAuto
) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Configurator
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Text
import qualified Data.Text               as T
import           Prelude                 hiding (lookup)
import           System.IO

withConfig :: [Worth FilePath] -> (Config -> IO ()) -> IO ()
withConfig path cb = do
  cfg <- try $ load path
  case cfg of
    Left (err :: SomeException) -> putStrLn $ show err
    Right cfg -> cb cfg

withConfigReload :: [Worth FilePath] -> (Config -> IO ()) -> IO ()
withConfigReload path cb = do
  v <- try $ autoReload autoConfig path
  case v of
    Left (err :: SomeException) -> putStrLn $ show err
    Right (cfg, tid) -> cb cfg

printCb :: Config -> IO ()
printCb cfg = do
  display cfg
  home <- lookup cfg "env.home" :: IO (Maybe Text)
  putStrLn $ show home
  pwd <- lookup cfg "env.pwd" :: IO (Maybe Text)
  putStrLn $ show pwd
  port <- lookup cfg "service.port" :: IO (Maybe Int)
  putStrLn $ show port
  return ()

-- | Dumps the Config forever, at ~1s intervals.
printCbForever :: Config -> IO ()
printCbForever cfg = do
  forever $ do
    display cfg
    threadDelay 1000000

defaultWorthFiles :: [Worth FilePath]
defaultWorthFiles = [Required "etc/tb-configurator.cfg"]

-- | This is to test the autoReload features.
-- run exampleAuto and then modify the values in /tmp/tb-configurator.cfg
tmpWorthFiles :: [Worth FilePath]
tmpWorthFiles = [Required "/tmp/tb-configurator.cfg"]

example = withConfig defaultWorthFiles printCb

-- | This example shows how we can automatically receive updated configuration
-- files without having to restart our handlers. Since this happens in the IO
-- monad, the background thread that configurator spawns gives us the latest
-- config.
exampleAuto = withConfigReload tmpWorthFiles printCbForever
