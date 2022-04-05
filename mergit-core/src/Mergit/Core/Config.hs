{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Mergit.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the ".lymerge.yaml" configuration file.
-}
module Mergit.Core.Config (
  BotConfig (..),
  configFileName,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)

data BotConfig = BotConfig
  { requiredStatuses :: [Text]
  }
  deriving (Show)

instance ToJSON BotConfig where
  toJSON BotConfig{..} =
    object
      [ "statuses" .= requiredStatuses
      ]

instance FromJSON BotConfig where
  parseJSON = withObject "BotConfig" $ \o ->
    BotConfig
      <$> o .: "statuses"

configFileName :: Text
configFileName = ".lymerge.yaml"
