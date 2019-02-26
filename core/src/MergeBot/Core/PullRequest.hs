{-|
Module      :  MergeBot.Core.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions to query and manage pull requests.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.PullRequest
  ( getPullRequests
  , getPullRequestDetail
  , getPullRequestSimple
  , getBranch
  , getBaseBranch
  ) where

import Data.GraphQL (get, runQuery, unwrap)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)

import MergeBot.Core.CIStatus (isPending)
import MergeBot.Core.Data
    ( BotStatus
    , CIStatus
    , MergeRun(..)
    , PullRequest(..)
    , PullRequestDetail(..)
    , PullRequestId
    , PullRequestSimple(..)
    , TryRun(..)
    )
import MergeBot.Core.GitHub (PaginatedResult(..), queryAll)
import MergeBot.Core.GraphQL.Enums.PullRequestReviewState
    (PullRequestReviewState(..))
import qualified MergeBot.Core.GraphQL.PullRequest as PullRequest
import qualified MergeBot.Core.GraphQL.PullRequestReview as PullRequestReview
import qualified MergeBot.Core.GraphQL.PullRequests as PullRequests
import MergeBot.Core.GraphQL.Scalars.DateTime (DateTime(..))
import MergeBot.Core.GraphQL.Scalars.GitObjectID (GitObjectID(..))
import MergeBot.Core.GraphQL.Scalars.HTML (HTML(..))
import MergeBot.Core.GraphQL.Scalars.URI (URI(..))
import MergeBot.Core.Monad (MonadBotApp(..), MonadGraphQL)

-- | Get all open pull requests.
getPullRequests :: (MonadBotApp m, MonadGraphQL m) => (PullRequestId -> BotStatus) -> m [PullRequest]
getPullRequests getStatus = do
  (_repoOwner, _repoName) <- getRepo
  queryAll $ \_after -> do
    result <- runQuery PullRequests.query PullRequests.Args{..}
    let info = [get| result.repository.pullRequests |]
        prs = [get| info.nodes![]! |]
        toPullRequest pr = PullRequest
          { number  = [get| pr.number |]
          , title   = [get| pr.title |]
          , author  = [get| pr.author!.login |]
          , created = unDateTime [get| pr.createdAt |]
          , updated = unDateTime [get| pr.updatedAt |]
          , status  = getStatus [get| pr.number |]
          }
    return PaginatedResult
      { chunk      = map toPullRequest prs
      , hasNext    = [get| info.pageInfo.hasNextPage |]
      , nextCursor = [get| info.pageInfo.endCursor |]
      }

type PR = [unwrap| (PullRequest.Schema).repository.pullRequest! |]

-- | Get the GraphQL result for a single pull request.
getPullRequest :: (MonadBotApp m, MonadGraphQL m) => PullRequestId -> m PR
getPullRequest prNum = do
  (_repoOwner, _repoName) <- getRepo
  let _number = prNum
  [get| .repository.pullRequest! |] <$> runQuery PullRequest.query PullRequest.Args{..}

-- | Get a simple pull request.
getPullRequestSimple :: MonadGraphQL m
  => PullRequestId -> m PullRequestSimple
getPullRequestSimple prNum = do
  pr <- getPullRequest prNum
  return PullRequestSimple
    { number = [get| pr.number |]
    , title  = [get| pr.title  |]
    }

-- | Get the branch name for the pull request.
getBranch :: MonadGraphQL m => PullRequestId -> m Text
getBranch = fmap [get| .headRefName |] . getPullRequest

-- | Get the base branch name for the pull request.
getBaseBranch :: MonadGraphQL m => PullRequestId -> m Text
getBaseBranch = fmap [get| .baseRefName |] . getPullRequest

-- | Get a detailed pull request.
getPullRequestDetail
  :: MonadGraphQL m
  => PullRequestId
  -> Maybe TryRun
  -> Maybe [PullRequestId]
  -> Maybe ([PullRequestId], CIStatus)
  -> m PullRequestDetail
getPullRequestDetail prNum tryRun maybeQueue maybeStaging = do
  pr <- getPullRequest prNum
  let baseRef = [get| pr.baseRefName |]

  reviews <- getReviews prNum
  mergeQueue <- traverse (mapM getPullRequestSimple) maybeQueue
  mergeRun <- case maybeStaging of
    Nothing -> return Nothing
    Just (staging, mergeStatus) -> do
      mergePRs <- mapM getPullRequestSimple staging
      return $ Just MergeRun{..}
  let approved = not (null reviews) && all (== APPROVED) reviews
      canTry = isNothing mergeRun && maybe True (not . isPending . tryStatus) tryRun
      canQueue = isNothing mergeQueue && isNothing mergeRun
      canUnqueue = isJust mergeQueue

  return PullRequestDetail
    { number  = [get| pr.number |]
    , title   = [get| pr.title |]
    , author  = [get| pr.author!.login |]
    , created = unDateTime [get| pr.createdAt |]
    , updated = unDateTime [get| pr.updatedAt |]
    , url     = unURI [get| pr.url |]
    , body    = unHTML [get| pr.bodyHTML |]
    , commit  = unOID [get| pr.headRefOid |]
    , ..
    }

-- | Get the reviews for the given pull request.
getReviews :: MonadGraphQL m => PullRequestId -> m [PullRequestReviewState]
getReviews prNum = do
  (_repoOwner, _repoName) <- getRepo
  fmap resolveReviews $ queryAll $ \_after -> do
    result <- runQuery PullRequestReview.query PullRequestReview.Args{_number=prNum,..}
    let info = [get| result.repository.pullRequest!.reviews! |]
        reviews = [get| info.nodes![]! |]
    return PaginatedResult
      { chunk      = map [get| .(author!.login, state) |] reviews
      , hasNext    = [get| info.pageInfo.hasNextPage |]
      , nextCursor = [get| info.pageInfo.endCursor |]
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
