{-|
Module      :  MergeBot.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the configuration for the merge bot.
-}

module MergeBot.Core.Config
  ( BotConfig(..)
  ) where

-- | Merge bot configuration.
data BotConfig = BotConfig
  { cfgRepoOwner :: String
  , cfgRepoName  :: String
  , cfgToken     :: String
  } deriving (Show)
