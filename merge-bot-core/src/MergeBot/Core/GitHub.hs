{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core.GitHub
  ( -- * Types
    Repo
  , PrNum
  , CommitSHA
  , BranchName
  , UserName
  , CheckRunType(..)
    -- * GraphQL
  , Tree
  , getBranchTree
  , getBranchSHA
  , CIContext
  , CICommit(..)
  , getCICommit
  , CheckRunId
  , getCheckRun
  , PRForCommit(..)
  , getPRForCommit
  , getPRReviews
  , isPRMerged
  , getQueues
    -- * REST
  , createCheckRun
  , updateCheckRun'
  , createCommit
  , createBranch
  , updateBranch
  , deleteBranch
  , mergeBranches
  , closePR
  ) where

import Control.Monad (forM, void)
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.GraphQL (get, mkGetter, runQuery, unwrap)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST
    ( GHEndpoint(..)
    , GitHubData
    , KeyValue(..)
    , StdMethod(..)
    , githubTry
    , githubTry'
    , (.:)
    )
import Network.HTTP.Types (status409)
import UnliftIO.Exception (throwIO)

import MergeBot.Core.Error (BotError(..))
import MergeBot.Core.GraphQL.API
    ( GetBranchSHAQuery(..)
    , GetBranchTreeQuery(..)
    , GetBranchTreeSchema
    , GetCICommitQuery(..)
    , GetCICommitSchema
    , GetIsPRMergedQuery(..)
    , GetPRCheckRunQuery(..)
    , GetPRForCommitQuery(..)
    , GetPRReviewsQuery(..)
    , GetQueuedPRsQuery(..)
    )
import MergeBot.Core.GraphQL.Enums.PullRequestReviewState
    (PullRequestReviewState)
import qualified MergeBot.Core.GraphQL.Enums.PullRequestReviewState as PullRequestReviewState
import MergeBot.Core.Monad (MonadMergeBot(..), queryGitHub')
import MergeBot.Core.Text (checkRunMerge, checkRunTry)

default (Text)

{- Types -}

type Repo = (Text, Text)
type PrNum = Int
type CommitSHA = GitObjectID
type BranchName = Text
type UserName = Text

data CheckRunType = CheckRunTry | CheckRunMerge

getCheckName :: CheckRunType -> Text
getCheckName = \case
  CheckRunTry -> checkRunTry
  CheckRunMerge -> checkRunMerge

{- GraphQL -}

mkGetter "Tree" "getTree" ''GetBranchTreeSchema ".repository!.ref!.target!.__fragment!.tree"

-- | Get the git tree for the given branch.
getBranchTree :: MonadMergeBot m => BranchName -> m Tree
getBranchTree branch = do
  (repoOwner, repoName) <- getRepo
  getTree <$>
    runQuery GetBranchTreeQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _name = branch
      }

-- | Get the git hash for the given branch, if it exists.
getBranchSHA :: MonadMergeBot m => BranchName -> m (Maybe CommitSHA)
getBranchSHA branch = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.ref?.target!.oid |] <$>
    runQuery GetBranchSHAQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _branch = branch
      }

type CIContext = [unwrap| GetCICommitSchema.repository!.object!.__fragment!.status!.contexts[] |]

data CICommit = CICommit
  { commitTree     :: Tree
  , commitContexts :: [CIContext]
  , parents        :: [(CommitSHA, CheckRunId)]
    -- ^ The parent commits of a CI commit, not including the base branch
  } deriving (Show)

-- | Get details for the given CI commit; that is, a commit created by 'createCIBranch'.
getCICommit :: MonadMergeBot m => CommitSHA -> CheckRunType -> m CICommit
getCICommit sha checkRunType = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  (result, parentsPayload) <- queryAll $ \after -> do
    result <- runQuery GetCICommitQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _appId = appId
      , _after = after
      , _sha = sha
      , _checkName = Just checkName
      }

    let payload = [get| result.repository!.object!.__fragment! |]
        info = [get| payload.parents.pageInfo |]

    return PaginatedResult
      { payload
      , chunk = [get| payload.parents.nodes![]! |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }

  parents <- forM
    (tail parentsPayload) -- ignore base branch, which is always first
    $ \parent -> do
      let getCheckRuns = [get| .checkSuites!.nodes![]!.checkRuns!.nodes![]!.databaseId! |]
          parentSHA = [get| parent.oid |]
      case concat $ getCheckRuns parent of
        [] -> throwIO $ MissingCheckRun parentSHA checkName
        [checkRun] -> return (parentSHA, checkRun)
        _ -> error $ "Commit has multiple check runs named '" ++ Text.unpack checkName ++ "': " ++ show parent

  return CICommit
    { commitTree = [get| result.tree |]
    , commitContexts = fromMaybe [] [get| result.status?.contexts |]
    , parents
    }
  where
    checkName = getCheckName checkRunType

type CheckRunId = Int

-- | Get the check run for the given PR and check run name.
getCheckRun :: MonadMergeBot m => PrNum -> CheckRunType -> m CheckRunId
getCheckRun prNum checkRunType = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  result <- runQuery GetPRCheckRunQuery
    { _repoOwner = repoOwner
    , _repoName = repoName
    , _prNum = prNum
    , _appId = appId
    , _checkName = checkName
    }
  commit <- case [get| result.repository!.pullRequest!.commits.nodes![]!.commit |] of
    [] -> error $ "PR #" ++ show prNum ++ " has no commits: " ++ show result
    [c] -> return c
    _ -> error $ "PRCheckRun query returned more than one 'last' commit: " ++ show result
  case [get| commit.checkSuites!.nodes![]!.checkRuns!.nodes![]!.databaseId! |] of
    [[checkRunId]] -> return checkRunId
    _ -> throwIO $ MissingCheckRunPR prNum checkName
  where
    checkName = getCheckName checkRunType

data PRForCommit = PRForCommit
  { prForCommitId         :: Int
  , prForCommitBaseBranch :: Text
  , prForCommitSHA        :: GitObjectID
  , prForCommitBranch     :: Text
  , prForCommitIsMerged   :: Bool
  }

-- | Get information for the associated PR for the given commit.
--
-- We expect the given commit to only be associated with one PR. The given commit does
-- not have to be the HEAD of the PR.
getPRForCommit :: MonadMergeBot m => GitObjectID -> m PRForCommit
getPRForCommit sha = do
  (repoOwner, repoName) <- getRepo

  result <- runQuery GetPRForCommitQuery
    { _repoOwner = repoOwner
    , _repoName = repoName
    , _sha = sha
    }

  let prs = [get| result.repository!.object!.__fragment!.associatedPullRequests!.nodes![]! |]

  case prs of
    [] -> throwIO $ CommitLacksPR sha
    [pr] -> return PRForCommit
      { prForCommitId = [get| pr.number |]
      , prForCommitBaseBranch = [get| pr.baseRefName |]
      , prForCommitSHA = [get| pr.headRefOid |]
      , prForCommitBranch = [get| pr.headRefName |]
      , prForCommitIsMerged = [get| pr.merged |]
      }
    _ -> throwIO $ CommitForManyPRs sha $ map [get| .number |] prs

-- | Return the reviews for the given PR as a map from reviewer to review state.
getPRReviews :: MonadMergeBot m => PrNum -> m (HashMap UserName PullRequestReviewState)
getPRReviews prNum = do
  (repoOwner, repoName) <- getRepo
  fmap (HashMap.fromListWith resolve) $ queryAll_ $ \after -> do
    result <- runQuery GetPRReviewsQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _prNum = prNum
      , _after = after
      }
    let payload = [get| result.repository!.pullRequest!.reviews! |]
        info = [get| payload.pageInfo |]
    return PaginatedResult
      { payload = ()
      , chunk = [get| payload.nodes![]!.(author!.login, state) |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }
  where
    -- NB: The final review state is the last review state a reviewer submitted, except prior
    -- APPROVED, DISMISSED, or CHANGES_REQUESTED states take precendence over later PENDING or
    -- COMMENTED states.
    resolve new old =
      let relevantStates =
            [ PullRequestReviewState.APPROVED
            , PullRequestReviewState.DISMISSED
            , PullRequestReviewState.CHANGES_REQUESTED
            ]
      in if old `elem` relevantStates && new `notElem` relevantStates
        then old
        else new

-- | Return True if the given PR is merged.
isPRMerged :: MonadMergeBot m => PrNum -> m Bool
isPRMerged prNum = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.pullRequest!.merged |] <$>
    runQuery GetIsPRMergedQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _prNum = prNum
      }

-- | Get all queued PRs, by base branch.
getQueues :: MonadMergeBot m => m (HashMap Text [(PrNum, CommitSHA, CheckRunId)])
getQueues  = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  fmap (HashMap.fromListWith (++)) $ queryAll_ $ \after -> do
    result <- runQuery GetQueuedPRsQuery
      { _repoOwner = repoOwner
      , _repoName = repoName
      , _after = after
      , _appId = appId
      , _checkName = checkRunMerge
      }
    let payload = [get| result.repository!.pullRequests |]
        info = [get| payload.pageInfo |]
    return PaginatedResult
      { payload = ()
      , chunk = mapMaybe getQueuedPR [get| payload.nodes![]! |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }
  where
    getQueuedPR pr =
      let prCommit = [get| pr.headRef!.target!.__fragment! |]
          (base, number, headRef) = [get| pr.(baseRefName, number, headRefOid) |]
      in case concat [get| prCommit.checkSuites!.nodes![]!.checkRuns!.nodes![]!.databaseId! |] of
        [] -> Nothing -- PR has no merge check run in the "queued" state
        checkRunId:_ -> Just (base, [(number, headRef, checkRunId)])

{- REST -}

-- | Create a check run.
--
-- https://developer.github.com/v3/checks/runs/#create-a-check-run
createCheckRun :: MonadMergeBot m => GitHubData -> m ()
createCheckRun ghData = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/check-runs"
  , endpointVals = []
  , ghData
  }

-- | Update a check run.
--
-- NOTE: Should NOT be run directly. Use MergeBot.Core.CheckRun.updateCheckRun instead.
--
-- https://developer.github.com/v3/checks/runs/#update-a-check-run
updateCheckRun' :: MonadMergeBot m => CheckRunId -> GitHubData -> m ()
updateCheckRun' checkRunId ghData = void $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/check-runs/:check_run_id"
  , endpointVals = ["check_run_id" := checkRunId]
  , ghData
  }

-- | Create a commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadMergeBot m => Text -> GitObjectID -> [CommitSHA] -> m CommitSHA
createCommit message tree parents = (.: "sha") <$> queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/commits"
  , endpointVals = []
  , ghData =
    [ "message" := message
    , "tree"    := tree
    , "parents" := parents
    ]
  }

-- | Create a branch.
--
-- https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadMergeBot m => BranchName -> CommitSHA -> m ()
createBranch name sha = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/refs"
  , endpointVals = []
  , ghData =
    [ "ref" := "refs/heads/" <> name
    , "sha" := sha
    ]
  }

-- | Set the given branch to the given commit.
--
-- Returns False if update is not a fast-forward.
--
-- https://developer.github.com/v3/git/refs/#update-a-reference
updateBranch :: MonadMergeBot m => Bool -> BranchName -> CommitSHA -> m (Either Text ())
updateBranch force branch sha = fmap resolve $ githubTry $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/git/refs/:ref"
  , endpointVals = ["ref" := "heads/" <> branch]
  , ghData = ["sha" := sha, "force" := force]
  }
  where
    resolve = bimap (.: "message") (const ())

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
-- https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: MonadMergeBot m => BranchName -> m ()
deleteBranch branch = void $ githubTry $ queryGitHub' GHEndpoint
  { method = DELETE
  , endpoint = "/repos/:owner/:repo/git/refs/:ref"
  , endpointVals = ["ref" := "heads/" <> branch]
  , ghData = []
  }

-- | Merge two branches, returning the merge commit information.
--
-- Returns False if there was a merge conflict
--
-- https://developer.github.com/v3/repos/merging/#perform-a-merge
mergeBranches :: MonadMergeBot m => BranchName -> CommitSHA -> Text -> m Bool
mergeBranches base sha message = fmap isRight $ githubTry' status409 $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/merges"
  , endpointVals = []
  , ghData =
    [ "base"           := base
    , "head"           := sha
    , "commit_message" := message
    ]
  }

-- | Close the given PR.
--
-- https://developer.github.com/v3/pulls/#update-a-pull-request
closePR :: MonadMergeBot m => PrNum -> m ()
closePR prNum = void $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/pulls/:number"
  , endpointVals = [ "number" := prNum ]
  , ghData = [ "state" := "closed" ]
  }

{- Helpers -}

data PaginatedResult payload a = PaginatedResult
  { payload    :: payload    -- ^ The full payload of the first page
  , chunk      :: [a]        -- ^ The paginated part of the payload
  , hasNext    :: Bool
  , nextCursor :: Maybe Text
  } deriving (Show)

-- | Run a paginated query as many times as possible until all the results have been fetched.
queryAll :: (Monad m, Show payload, Show a)
  => (Maybe Text -> m (PaginatedResult payload a)) -> m (payload, [a])
queryAll doQuery = queryAll' Nothing
  where
    queryAll' cursor = do
      result@PaginatedResult{..} <- doQuery cursor
      (_, next) <- case (hasNext, nextCursor) of
        (True, Just nextCursor') -> queryAll' $ Just nextCursor'
        (True, Nothing) -> error $ "Paginated result says it has next with no cursor: " ++ show result
        (False, _) -> return (payload, [])
      return (payload, chunk ++ next)

queryAll_ :: (Monad m, Show a)
  => (Maybe Text -> m (PaginatedResult () a)) -> m [a]
queryAll_ = fmap snd . queryAll
