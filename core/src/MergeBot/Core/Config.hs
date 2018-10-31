{-|
Module      :  MergeBot.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the configuration for the merge bot.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module MergeBot.Core.Config
  ( BotConfig(..)
  , BranchConfig(..)
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Merge bot configuration.
data BotConfig = BotConfig
  { cfgRepoOwner :: String
  , cfgRepoName  :: String
  , cfgToken     :: String
  } deriving (Show)

-- | In-repo configuration.
data BranchConfig = BranchConfig
  { requiredStatuses :: [Text]
  } deriving (Generic,FromJSON)
