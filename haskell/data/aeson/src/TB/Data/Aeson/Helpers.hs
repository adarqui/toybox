{-# LANGUAGE OverloadedStrings #-}

module TB.Data.Aeson.Helpers (
  properJSON,
  properLensJSON
) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char

-- | Transforms normal camelCase records into "proper json" strings
--
-- >>> camelTo '_' "camelCase"
-- "camel_case"
properJSON = defaultOptions {
  fieldLabelModifier = camelTo '_'
}

-- | Transforms camelCase "lens" records into "proper json" strings
--
-- >>> (camelTo '_' . drop 1) "_camelCase"
-- "camel_case"
properLensJSON = defaultOptions {
  fieldLabelModifier = camelTo '_' . drop 1
}
