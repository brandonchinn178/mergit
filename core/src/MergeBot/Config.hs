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

import MergeBot.Diff (DiffOptions(..), DiffOptionsFull)
import MergeBot.Merge (MergeAlgorithm(..))

-- | The configuration of the merge bot.
data BotConfig = BotConfig
  { defaultDiffOptions :: DiffOptionsFull
  } deriving (Show)

-- | The default configuration.
defaultConfig :: BotConfig
defaultConfig = BotConfig
  { defaultDiffOptions = DiffOptions
    { mergeAlgorithm = Merge
    }
  }
