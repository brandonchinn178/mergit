{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MergeBot.Core
  ( listPullRequests
  , getPullRequest
  , tryPullRequest
  , queuePullRequest
  , unqueuePullRequest
  , startMergeJob
  , runMerge
  ) where

import Control.Monad (forM_)
import Control.Monad.Reader (asks)
import Data.Map.Strict ((!?))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import MergeBot.Core.Branch
import MergeBot.Core.Data
import MergeBot.Core.GitHub
import MergeBot.Core.Monad
import MergeBot.Core.PullRequest
import MergeBot.Core.State

-- | List all open pull requests.
listPullRequests :: MonadGraphQL m => BotState -> m [PullRequest]
listPullRequests state = do
  branchStatuses <- getBranchStatuses $ Set.toList $ getMergeQueue state
  getPullRequests $ fromMaybe None . (branchStatuses !?)

-- | Return a single pull request.
getPullRequest :: MonadGraphQL m => BotState -> PullRequestId -> m PullRequestDetail
getPullRequest state prNum = do
  prTryRun <- fmap TryRun <$> getTryStatus prNum
  staging <- getStagingPRs
  maybeStaging <- if prNum `elem` staging
    then fmap (staging,) <$> getStagingStatus
    else return Nothing
  getPullRequestDetail prNum prTryRun maybeQueue maybeStaging
  where
    queue = getMergeQueue state
    maybeQueue = if prNum `Set.member` queue
      then Just $ Set.toList queue
      else Nothing

-- | Start a try job for the given pull request.
tryPullRequest :: (MonadGraphQL m, MonadREST m) => PullRequestId -> m ()
tryPullRequest = createTryBranch

-- | Queue the given pull request.
queuePullRequest :: PullRequestId -> BotState -> BotState
queuePullRequest = insertMergeQueue

-- | Unqueue the given pull request.
unqueuePullRequest :: PullRequestId -> BotState -> BotState
unqueuePullRequest = removeMergeQueue

-- | Start a merge job.
startMergeJob :: (MonadGraphQL m, MonadREST m) => BotState -> m BotState
startMergeJob state = do
  createMergeBranch $ Set.toList queue
  return $ clearMergeQueue state
  where
    queue = getMergeQueue state

-- | Merge pull requests after a successful merge job.
runMerge :: (MonadGraphQL m, MonadREST m) => m ()
runMerge = do
  (_repoOwner, _repoName) <- asks getRepo
  mergeStaging >>= \case
    -- TODO: handle master being different than when staging started
    Nothing -> fail "Update was not a fast-forward"
    Just prs -> forM_ prs $ \prNum -> do
      deleteBranch =<< getBranch prNum
      deleteTryBranch prNum
