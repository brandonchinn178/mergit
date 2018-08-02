{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot
  ( startMergeJob
  , execMerge
  ) where

import qualified Data.Set as Set

import MergeBot.Monad
import MergeBot.State

-- | Start a merge job with all the diffs in the merge queue.
startMergeJob :: (MonadGHBranch m, MonadGHPullRequest m) => BotState -> m BotState
startMergeJob state = do
  let state' = initMergeJob state
  deleteBranch "staging"
  createBranch "staging"
  mergeBranches "staging" $ Set.toList $ getMergeJobs state'
  return state'

-- | Execute a merge after a successful CI run.
execMerge :: MonadGHPromote m => BotState -> m (Either String BotState)
execMerge state = do
  canPromote <- canPromoteStaging
  if canPromote
    then do
      promoteStaging
      return $ Right $ clearMergeJobs state
    else return $ Left "Could not promote 'staging' to 'master'"
