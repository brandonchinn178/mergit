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
import MergeBot.Patch (PatchOptions(..), PatchOptionsFull)

-- | The configuration of the merge bot.
data BotConfig = BotConfig
  { defaultPatchOptions :: PatchOptionsFull
  } deriving (Show)

-- | The default configuration.
defaultConfig :: BotConfig
defaultConfig = BotConfig
  { defaultPatchOptions = PatchOptions
    { mergeAlgorithm = Merge
    }
  }
