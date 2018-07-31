{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}

module MergeBot
  ( execMerge
  ) where

-- | Execute a merge after a successful CI run.
execMerge :: MonadGHBranch m => BotConfig -> BotState -> m BotState
execMerge = undefined
