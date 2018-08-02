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

import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

import MergeBot.Config
import MergeBot.Diff
import MergeBot.Monad.Class
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
execMerge :: MonadGHPullRequest m => BotConfig -> BotState -> m BotState
execMerge BotConfig{..} state = do
  forM_ (getMergeJobs state) $ \diff ->
    let options = fromJust $ getDiffOptions state diff
        DiffOptions{..} = resolveOptions options defaultDiffOptions
    in mergePullRequest diff mergeAlgorithm
  return $ clearMergeJobs state
