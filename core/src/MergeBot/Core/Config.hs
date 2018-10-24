{-|
Module      :  MergeBot.Core.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the configuration for the merge bot.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Config
  ( BotConfig(..)
  , getRepo
  ) where

-- | Merge bot configuration.
data BotConfig = BotConfig
  { repoOwner   :: String
  , repoName    :: String
  , githubToken :: String
  } deriving (Show)

-- | A helper to get the repoOwner and repoName as a pair.
getRepo :: BotConfig -> (String, String)
getRepo BotConfig{..} = (repoOwner, repoName)
