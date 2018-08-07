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
  ( startMergeJob
  , execMerge
  ) where

import Data.Foldable (forM_)
import Data.Maybe (fromJust)

import MergeBot.Config
import MergeBot.Patch
import MergeBot.Monad.Class
import MergeBot.State

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
execMerge :: MonadGHPullRequest m => BotConfig -> BotState -> m BotState
execMerge BotConfig{..} state = do
  forM_ (getMergeJobs state) $ \patch ->
    let options = fromJust $ getPatchOptions state patch
        PatchOptions{..} = resolveOptions options defaultPatchOptions
    in mergePullRequest patch mergeAlgorithm
  return $ clearMergeJobs state
