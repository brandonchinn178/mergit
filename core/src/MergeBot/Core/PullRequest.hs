{-|
Module      :  MergeBot.Core.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions to query and manage pull requests.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.PullRequest
  ( getPullRequests
  , getPullRequestDetail
  , getPullRequestSimple
  , getBranch
  ) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Object)
import Data.GraphQL (MonadQuery, runQuery)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)

import MergeBot.Core.CIStatus (isPending)
import MergeBot.Core.Data
    ( BotStatus
    , PullRequest(..)
    , PullRequestDetail(..)
    , PullRequestId
    , PullRequestSimple(..)
    , TryRun(..)
    )
import MergeBot.Core.GitHub (PaginatedResult(..), queryAll)
import qualified MergeBot.Core.GraphQL.PullRequest as PullRequest
import qualified MergeBot.Core.GraphQL.PullRequestReview as PullRequestReview
import MergeBot.Core.GraphQL.PullRequestReviewState (PullRequestReviewState(..))
import qualified MergeBot.Core.GraphQL.PullRequests as PullRequests
import MergeBot.Core.GraphQL.Scalars (parseUTCTime)
import MergeBot.Core.Monad (BotEnv, getRepo)

-- | Get all open pull requests.
getPullRequests :: (MonadReader BotEnv m, MonadQuery m) =>
  (PullRequestId -> BotStatus) -> m [PullRequest]
getPullRequests getStatus = do
  (_repoOwner, _repoName) <- asks getRepo
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
            , status  = getStatus prNum
            }
    return PaginatedResult
      { chunk      = map toPullRequest prs
      , hasNext    = [PullRequests.get| @info.pageInfo.hasNextPage |]
      , nextCursor = [PullRequests.get| @info.pageInfo.endCursor |]
      }

-- | Get the GraphQL result for a single pull request.
getPullRequest :: (MonadReader BotEnv m, MonadQuery m) => PullRequestId -> m Object
getPullRequest prNum = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery PullRequest.query PullRequest.Args{_number=prNum, ..}
  return [PullRequest.get| result.repository.pullRequest! > pr |]

-- | Get a simple pull request.
getPullRequestSimple :: (MonadReader BotEnv m, MonadQuery m)
  => PullRequestId -> m PullRequestSimple
getPullRequestSimple prNum = do
  pr <- getPullRequest prNum
  return PullRequestSimple
    { number = [PullRequest.get| @pr.number |]
    , title  = [PullRequest.get| @pr.title  |]
    }

-- | Get the branch name for the pull request.
getBranch :: (MonadReader BotEnv m, MonadQuery m)
  => PullRequestId -> m Text
getBranch prNum = do
  pr <- getPullRequest prNum
  return [PullRequest.get| @pr.headRefName |]

-- | Get a detailed pull request.
getPullRequestDetail :: (MonadReader BotEnv m, MonadQuery m)
  => PullRequestId -> Maybe TryRun -> Maybe [PullRequestId] -> m PullRequestDetail
getPullRequestDetail prNum tryRun maybeQueue = do
  (_repoOwner, _repoName) <- asks getRepo

  pr <- getPullRequest prNum
  let base = [PullRequest.get| @pr.baseRefName |]

  reviews <- getReviews prNum
  mergeQueue <- traverse (mapM getPullRequestSimple) maybeQueue
  let approved = not (null reviews) && all (== APPROVED) reviews
      mergeRun = Nothing -- TODO: get PRs in staging branch and the status of the merge run
      canTry = isNothing mergeRun && maybe True (not . isPending . tryStatus) tryRun
      canQueue = isNothing mergeQueue && isNothing mergeRun && base == "master"
      canUnqueue = isJust mergeQueue

  return PullRequestDetail
    { number  = [PullRequest.get| @pr.number |]
    , title   = [PullRequest.get| @pr.title |]
    , author  = [PullRequest.get| @pr.author!.login |]
    , created = parseUTCTime [PullRequest.get| @pr.createdAt |]
    , updated = parseUTCTime [PullRequest.get| @pr.updatedAt |]
    , url     = [PullRequest.get| @pr.url |]
    , body    = [PullRequest.get| @pr.bodyHTML |]
    , commit  = [PullRequest.get| @pr.headRefOid |]
    , ..
    }

-- | Get the reviews for the given pull request.
getReviews :: (MonadReader BotEnv m, MonadQuery m) => PullRequestId -> m [PullRequestReviewState]
getReviews prNum = do
  (_repoOwner, _repoName) <- asks getRepo
  fmap resolveReviews $ queryAll $ \_after -> do
    result <- runQuery PullRequestReview.query PullRequestReview.Args{_number=prNum,..}
    let info = [PullRequestReview.get| result.repository.pullRequest!.reviews! > info |]
        reviews = [PullRequestReview.get| @info.nodes![]! > review |]
        fromReview review =
          ( [PullRequestReview.get| @review.author!.login |]
          , [PullRequestReview.get| @review.state |]
          )
    return PaginatedResult
      { chunk      = map fromReview reviews
      , hasNext    = [PullRequestReview.get| @info.pageInfo.hasNextPage |]
      , nextCursor = [PullRequestReview.get| @info.pageInfo.endCursor |]
      }

{- Helpers -}

-- | Resolve the given pull request reviews according to the given rules:
--
-- * Only track APPROVED or CHANGES_REQUESTED reviews
-- * A given author's reviews later in the list overrule their reviews earlier in the list
resolveReviews :: [(Text, PullRequestReviewState)] -> [PullRequestReviewState]
resolveReviews = squashReviews' Map.empty
  where
    squashReviews' reviews [] = Map.elems reviews
    squashReviews' reviews ((author, review):rest) =
      let reviews' = if review `elem` [APPROVED, CHANGES_REQUESTED]
            then Map.insert author review reviews
            else reviews
      in squashReviews' reviews' rest
