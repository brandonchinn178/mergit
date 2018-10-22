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
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)

import MergeBot.Core.Branch
import MergeBot.Core.Data
import MergeBot.Core.GitHub (queryAll)
import qualified MergeBot.Core.GraphQL.PullRequest as PullRequest
import qualified MergeBot.Core.GraphQL.PullRequestReview as PullRequestReview
import MergeBot.Core.GraphQL.PullRequestReviewState (PullRequestReviewState(..))
import qualified MergeBot.Core.GraphQL.PullRequests as PullRequests
import qualified MergeBot.Core.GraphQL.PullRequestSimple as PullRequestSimple
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
getPullRequest state _number = do
  result <- runQuery PullRequest.query PullRequest.Args{..}
  let pr = [PullRequest.get| result.repository.pullRequest! > pr |]
  reviews <- resolveReviews <$> queryAll getReviews
  tryStatus <- getTryStatus state _number
  queue <- if _number `Set.member` mergeQueue
    then fmap Just $ mapM getSimplePullRequest $ Set.toList mergeQueue
    else return Nothing
  let mergeRun = Nothing -- TODO: get PRs in staging branch and the status of the merge run
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
    , approved    = not (null reviews) && all (== APPROVED) reviews
    , tryRun      = TryRun <$> tryStatus
    , mergeQueue  = queue
    , mergeRun    = mergeRun
    , canTry      = isNothing mergeRun && isNothing tryStatus
    , canQueue    = isNothing queue
    , canUnqueue  = isJust queue
    }
  where
    (_repoOwner, _repoName) = getRepo state
    mergeQueue = getMergeQueue state
    getReviews _after = do
      result <- runQuery PullRequestReview.query PullRequestReview.Args{..}
      let info = [PullRequestReview.get| result.repository.pullRequest!.reviews! > info |]
          reviews = [PullRequestReview.get| @info.nodes![]! > review |]
          fromReview review =
            ( [PullRequestReview.get| @review.author!.login |]
            , [PullRequestReview.get| @review.state |]
            )
      return
        ( map fromReview reviews
        , [PullRequestReview.get| @info.pageInfo.hasNextPage |]
        , [PullRequestReview.get| @info.pageInfo.endCursor |]
        )
    getSimplePullRequest num = do
      result <- runQuery PullRequestSimple.query PullRequestSimple.Args{_number=num, ..}
      let pr = [PullRequestSimple.get| result.repository.pullRequest! > pr |]
      return PullRequestSimple
        { number = [PullRequestSimple.get| @pr.number |]
        , title  = [PullRequestSimple.get| @pr.title |]
        }

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
