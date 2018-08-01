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

import MergeBot.Monad
import MergeBot.State

-- | Execute a merge after a successful CI run.
execMerge :: MonadGHPromote m => BotState -> m (Either String BotState)
execMerge state = do
  canPromote <- canPromoteStaging
  if canPromote
    then do
      promoteStaging
      return $ Right $ clearMergeJobs state
    else return $ Left "Could not promote 'staging' to 'master'"
