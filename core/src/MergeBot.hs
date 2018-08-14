{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot
  ( addMergeQueue
  , startMergeJob
  , execMerge
  ) where

import Data.Foldable (forM_)
import Data.Maybe (fromJust)

import MergeBot.Config
import MergeBot.Error
import MergeBot.Monad.Class
import MergeBot.Patch
import MergeBot.State

-- | Add the given pull request to the merge queue.
addMergeQueue :: (MonadGHPullRequest m) =>
  PatchId -> PatchOptionsPartial -> BotState -> m BotState
addMergeQueue patch options state = do
  approved <- isApproved patch
  if approved
    then either fail' return $ insertMergeQueue patch options state
    else fail' PatchNotApproved -- TODO: add to holding queue
  where
    fail' e = do
      -- TODO: post comment
      return state

-- | Start a merge job with all the pull requests in the merge queue.
startMergeJob :: (MonadGHBranch m, MonadGHPullRequest m) => BotState -> m BotState
startMergeJob state = do
  let state' = initMergeJob state
  deleteBranch "staging"
  createBranch "staging"
  forM_ (getMergeJobs state') $ \patchId ->
    getBranch patchId >>= \case
      Nothing -> fail $ "Could not find pull request #" ++ show patchId
      Just branch -> mergeBranch "staging" branch
  return state'

-- | Execute a merge after a successful CI run.
execMerge :: (MonadGHBranch m, MonadGHPullRequest m) => BotConfig -> BotState -> m BotState
execMerge BotConfig{..} state = do
  forM_ (getMergeJobs state) $ \patch ->
    let options = fromJust $ getPatchOptions state patch
        PatchOptions{..} = resolveOptions options defaultPatchOptions
    in mergePullRequest patch mergeAlgorithm
  deleteBranch "staging"
  return $ clearMergeJobs state
