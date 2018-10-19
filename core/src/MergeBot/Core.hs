{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core
  ( listPullRequests
  , getPullRequest
  , tryPullRequest
  , queuePullRequest
  , unqueuePullRequest
  , startMergeJob
  , runMerge
  ) where

import Data.GraphQL (MonadQuery, runQuery)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import MergeBot.Core.Branch
import MergeBot.Core.Data
import qualified MergeBot.Core.GraphQL.PullRequests as PullRequests
import MergeBot.Core.GraphQL.Scalars (parseUTCTime)
import MergeBot.Core.State

-- | List all open pull requests.
listPullRequests :: MonadQuery m => BotState -> m [PullRequest]
listPullRequests state = do
  branchStatuses <- getBranchStatuses state
  queryPullRequests branchStatuses Nothing
  where
    (_repoOwner, _repoName) = getRepo state
    queryPullRequests branchStatuses _after = do
      result <- runQuery PullRequests.query PullRequests.Args{..}
      let info = [PullRequests.get| result.repository.pullRequests > info |]
          pageInfo = [PullRequests.get| @info.pageInfo > pageInfo |]
          prs = [PullRequests.get| @info.nodes![]! > pr |]
          toPullRequest pr =
            let prNum = [PullRequests.get| @pr.number |]
            in PullRequest
              { number  = prNum
              , title   = [PullRequests.get| @pr.title |]
              , author  = [PullRequests.get| @pr.author!.login |]
              , created = parseUTCTime [PullRequests.get| @pr.createdAt |]
              , updated = parseUTCTime [PullRequests.get| @pr.updatedAt |]
              , status  = fromMaybe None $ Map.lookup prNum branchStatuses
              }
      next <- if [PullRequests.get| @pageInfo.hasNextPage |]
        then queryPullRequests branchStatuses $
          Just $ Text.unpack [PullRequests.get| @pageInfo.endCursor! |]
        else return []
      return $ map toPullRequest prs ++ next

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
