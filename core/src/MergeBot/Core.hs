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

import MergeBot.Core.Branch
import MergeBot.Core.Data
import MergeBot.Core.GitHub (queryAll)
import qualified MergeBot.Core.GraphQL.PullRequest as PullRequest
import qualified MergeBot.Core.GraphQL.PullRequests as PullRequests
import MergeBot.Core.GraphQL.Scalars (parseUTCTime)
import MergeBot.Core.State

-- | List all open pull requests.
listPullRequests :: MonadQuery m => BotState -> m [PullRequest]
listPullRequests state = do
  branchStatuses <- getBranchStatuses state
  queryAll $ \_after -> do
    result <- runQuery PullRequests.query PullRequests.Args{..}
    let info = [PullRequests.get| result.repository.pullRequests > info |]
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
    return
      ( map toPullRequest prs
      , [PullRequests.get| @info.pageInfo.hasNextPage |]
      , [PullRequests.get| @info.pageInfo.endCursor |]
      )
  where
    (_repoOwner, _repoName) = getRepo state

-- | Return a single pull request.
getPullRequest :: MonadQuery m => BotState -> PullRequestId -> m PullRequestDetail
getPullRequest state prNum = do
  result <- runQuery PullRequest.query PullRequest.Args{_number = prNum, ..}
  let pr = [PullRequest.get| result.repository.pullRequest! > pr |]
  return PullRequestDetail
    { number      = [PullRequest.get| @pr.number |]
    , title       = [PullRequest.get| @pr.title |]
    , author      = [PullRequest.get| @pr.author!.login |]
    , created     = parseUTCTime [PullRequest.get| @pr.createdAt |]
    , updated     = parseUTCTime [PullRequest.get| @pr.updatedAt |]
    , url         = [PullRequest.get| @pr.url |]
    , body        = [PullRequest.get| @pr.bodyHTML |]
    , commit      = [PullRequest.get| @pr.headRefOid |]
    , base        = [PullRequest.get| @pr.baseRefName |]
    , approved    = error "approved"
    , tryRun      = error "tryRun"
    , mergeQueue  = error "mergeQueue"
    , mergeRun    = error "mergeRun"
    , canTry      = error "canTry"
    , canQueue    = error "canQueue"
    , canUnqueue  = error "canUnqueue"
    }
  where
    (_repoOwner, _repoName) = getRepo state

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
