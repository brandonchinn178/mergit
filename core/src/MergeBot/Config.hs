{-|
Module      :  MergeBot.Config
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the configuration of the merge bot.
-}

module MergeBot.Config
  ( BotConfig(..)
  , defaultConfig
  ) where

import MergeBot.Merge (MergeAlgorithm(..))

-- | The configuration of the merge bot.
data BotConfig = BotConfig
  { defaultMergeAlgorithm :: MergeAlgorithm
  } deriving (Show)

-- | The default configuration.
defaultConfig :: BotConfig
defaultConfig = BotConfig
  { defaultMergeAlgorithm = Merge
  }
