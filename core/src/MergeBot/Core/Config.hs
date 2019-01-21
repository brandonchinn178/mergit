{-|
Module      :  MergeBot.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the configuration for the merge bot.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Config
  ( BotConfig(..)
  , BranchConfig(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import Data.Text (Text)

-- | Merge bot configuration.
data BotConfig = BotConfig
  { cfgRepoOwner :: String
  , cfgRepoName  :: String
  , cfgToken     :: String
  } deriving (Show)

-- | In-repo configuration.
data BranchConfig = BranchConfig
  { requiredStatuses :: [Text]
  } deriving (Show)

instance ToJSON BranchConfig where
  toJSON BranchConfig{..} = object
    [ "statuses" .= requiredStatuses
    ]

instance FromJSON BranchConfig where
  parseJSON = withObject "BranchConfig" $ \o ->
    BranchConfig
      <$> o .: "statuses"
