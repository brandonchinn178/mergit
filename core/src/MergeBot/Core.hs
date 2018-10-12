{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}

module MergeBot.Core
  ( listPullRequests
  , getPullRequest
  , tryPullRequest
  , queuePullRequest
  , unqueuePullRequest
  , startMergeJob
  , runMerge
  ) where

import MergeBot.Core.Data
import MergeBot.Core.State

-- | List all open pull requests.
listPullRequests :: Monad m => BotState -> m [PullRequest]
listPullRequests = undefined

-- | Return a single pull request.
getPullRequest :: Monad m => PullRequestId -> m PullRequestDetail
getPullRequest = undefined

-- | Start a try job for the given pull request.
tryPullRequest :: Monad m => PullRequestId -> m ()
tryPullRequest = undefined

-- | Queue the given pull request.
queuePullRequest :: PullRequestId -> BotState -> BotState
queuePullRequest = insertMergeQueue

-- | Unqueue the given pull request.
unqueuePullRequest :: PullRequestId -> BotState -> BotState
unqueuePullRequest = removeMergeQueue

-- | Start a merge job.
startMergeJob :: Monad m => BotState -> m BotState
startMergeJob = undefined

-- | Merge pull requests after a successful merge job.
runMerge :: Monad m => m ()
runMerge = undefined
