{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      :  Mergit.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the ".mergit.yaml" configuration file.
-}
module Mergit.Core.Config (
  MergitConfig (..),
  configFileName,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)

data MergitConfig = MergitConfig
  { requiredStatuses :: [Text]
  }
  deriving (Show)

instance ToJSON MergitConfig where
  toJSON MergitConfig{..} =
    object
      [ "statuses" .= requiredStatuses
      ]

instance FromJSON MergitConfig where
  parseJSON = withObject "MergitConfig" $ \o ->
    MergitConfig
      <$> o .: "statuses"

configFileName :: Text
configFileName = ".mergit.yaml"
