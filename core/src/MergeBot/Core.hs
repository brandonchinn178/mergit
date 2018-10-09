{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core
  ( addMergeQueue
  , startMergeJob
  , execMerge
  ) where

import Data.Foldable (forM_)

import MergeBot.Core.Merge
import MergeBot.Core.Monad.Class
import MergeBot.Core.Patch
import MergeBot.Core.State

-- | Add the given pull request to the merge queue.
addMergeQueue :: (MonadGHPullRequest m) =>
  PatchId -> BotState -> m BotState
addMergeQueue patch state = do
  approved <- isApproved patch
  if approved
    then return $ insertMergeQueue patch state
    else fail "Could not add to merge queue" -- TODO: add to holding queue, delete PatchNotApproved

-- | Start a merge job with all the pull requests in the merge queue.
startMergeJob :: (MonadGHBranch m, MonadGHPullRequest m) => BotState -> m BotState
startMergeJob state = do
  let state' = initMergeJob state
  deleteBranch "staging"
  createBranch "staging"
  forM_ (getMergeJobs state') $ \patch ->
    getBranch patch >>= \case
      Nothing -> fail $ "Could not find pull request #" ++ show patch
      Just branch -> mergeBranch "staging" branch
  return state'

-- | Execute a merge after a successful CI run.
execMerge :: (MonadGHBranch m, MonadGHPullRequest m) => BotState -> m BotState
execMerge state = do
  forM_ (getMergeJobs state) $ \patch -> mergePullRequest patch Merge
  deleteBranch "staging"
  return $ clearMergeJobs state
