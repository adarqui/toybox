{-# LANGUAGE OverloadedStrings #-}

module TB.Containers.Map.Examples (
  module A,
  loadServices
) where

import           Control.Applicative
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           TB.Attoparsec.Parsers.Services as A

loadServices :: [Service] -> M.Map Text Service
loadServices = M.fromList . map (\service -> (serviceName service, service))

loadServicesIO :: IO (M.Map Text Service)
loadServicesIO = loadServices <$> parseServicesIO
