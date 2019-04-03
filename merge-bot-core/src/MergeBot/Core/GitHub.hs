{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core.GitHub
  ( -- * GraphQL
    Tree
  , getBranchTree
  , getBranchSHA
  , CIContext
  , CICommit(..)
  , getCICommit
  , getCheckRun
  , getPRForCommit
  , getPRReviews
  , isPRMerged
  , getQueues
    -- * REST
  , createCheckRun
  , updateCheckRun
  , createCommit
  , createBranch
  , updateBranch
  , deleteBranch
  , mergeBranches
  , closePR
  ) where

import Control.Monad (forM, void)
import Data.Either (isRight)
import Data.GraphQL (get, mkGetter, runQuery, unwrap)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.PullRequestReviewState (PullRequestReviewState(..))
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
import qualified MergeBot.Core.GraphQL.BranchSHA as BranchSHA
import qualified MergeBot.Core.GraphQL.BranchTree as BranchTree
import qualified MergeBot.Core.GraphQL.CICommit as CICommit
import qualified MergeBot.Core.GraphQL.PRCheckRun as PRCheckRun
import qualified MergeBot.Core.GraphQL.PRForCommit as PRForCommit
import qualified MergeBot.Core.GraphQL.PRIsMerged as PRIsMerged
import qualified MergeBot.Core.GraphQL.PRReviews as PRReviews
import qualified MergeBot.Core.GraphQL.QueuedPRs as QueuedPRs
import MergeBot.Core.Monad (MonadMergeBot(..), queryGitHub')
import MergeBot.Core.Text (checkRunMerge)

default (Text)

type CheckRunId = Int

{- GraphQL -}

mkGetter "Tree" "getTree" ''BranchTree.Schema ".repository.ref!.target.tree!"

-- | Get the git tree for the given branch.
getBranchTree :: MonadMergeBot m => Text -> m Tree
getBranchTree branch = do
  (repoOwner, repoName) <- getRepo
  getTree <$>
    runQuery BranchTree.query BranchTree.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _name = Text.unpack branch
      }

-- | Get the git hash for the given branch, if it exists.
getBranchSHA :: MonadMergeBot m => Text -> m (Maybe GitObjectID)
getBranchSHA branch = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.ref?.target.oid |] <$>
    runQuery BranchSHA.query BranchSHA.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _branch = Text.unpack branch
      }

type CIContext = [unwrap| (CICommit.Schema).repository!.object!.status!.contexts[] |]

data CICommit = CICommit
  { commitTree     :: Tree
  , commitContexts :: [CIContext]
  , parents        :: [(GitObjectID, CheckRunId)]
    -- ^ The parent commits of a CI commit, not including the base branch
  }

-- | Get details for the given CI commit; that is, a commit created by 'createCIBranch'.
getCICommit :: MonadMergeBot m => GitObjectID -> Text -> m CICommit
getCICommit sha checkName = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  (result, parents) <- queryAll $ \after -> do
    result <- runQuery CICommit.query CICommit.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _appId = appId
      , _after = after
      , _sha = sha
      , _checkName = Just $ Text.unpack checkName
      }
    let payload = [get| result.repository!.object! |]
        info = [get| payload.parents!.pageInfo |]
        -- ignore base branch, which is always first
        parents = tail [get| payload.parents!.nodes![]! |]

    chunk <- forM (tail parents) $ \parent -> do
      let getCheckRuns = [get| .checkSuites!.nodes![]!.checkRuns!.nodes![]!.databaseId |]
          parentSHA = [get| parent.oid |]
      case concat $ getCheckRuns parent of
        [] -> throwIO $ MissingCheckRun parentSHA checkName
        [checkRun] -> return (parentSHA, checkRun)
        _ -> fail $ "Commit has multiple check runs named '" ++ Text.unpack checkName ++ "': " ++ show parent

    return PaginatedResult
      { payload
      , chunk
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }

  return CICommit
    { commitTree = [get| result.tree! |]
    , commitContexts = fromMaybe [] [get| result.status?.contexts |]
    , parents
    }

-- | Get the check run for the given PR and check run name.
getCheckRun :: MonadMergeBot m => Int -> Text -> m CheckRunId
getCheckRun prNum checkName = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  result <- runQuery PRCheckRun.query PRCheckRun.Args
    { _repoOwner = Text.unpack repoOwner
    , _repoName = Text.unpack repoName
    , _prNum = prNum
    , _appId = appId
    , _checkName = Text.unpack checkName
    }
  commit <- case [get| result.repository!.pullRequest!.commits.nodes![]!.commit |] of
    [] -> fail $ "PR #" ++ show prNum ++ " has no commits: " ++ show result
    [c] -> return c
    _ -> fail $ "PRCheckRun query returned more than one 'last' commit: " ++ show result
  case [get| commit.checkSuites!.nodes![]!.checkRuns!.nodes![]!.databaseId |] of
    [[checkRunId]] -> return checkRunId
    _ -> throwIO $ MissingCheckRunPR prNum checkName

-- | Get the PR number and branch name for the given commit.
getPRForCommit :: MonadMergeBot m => GitObjectID -> m (Int, Text)
getPRForCommit sha = do
  (repoOwner, repoName) <- getRepo
  result <- queryAll_ $ \after -> do
    result <- runQuery PRForCommit.query PRForCommit.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _sha = sha
      , _after = after
      }
    let payload = [get| result.repository!.object!.associatedPullRequests! |]
        info = [get| payload.pageInfo |]
    return PaginatedResult
      { payload = ()
      , chunk = [get| payload.nodes![]! |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }
  case filter ((== sha) . [get| .headRefOid |]) result of
    [] -> throwIO $ CommitLacksPR sha
    [pr] -> return [get| pr.(number, headRef!.name) |]
    prs -> throwIO $ CommitForManyPRs sha $ map [get| .number |] prs

-- | Return the reviews for the given PR as a map from reviewer to review state.
getPRReviews :: MonadMergeBot m => Int -> m (HashMap Text PullRequestReviewState)
getPRReviews prNum = do
  (repoOwner, repoName) <- getRepo
  fmap (HashMap.fromListWith resolve) $ queryAll_ $ \after -> do
    result <- runQuery PRReviews.query PRReviews.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
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
      let relevantStates = [APPROVED, DISMISSED, CHANGES_REQUESTED]
      in if old `elem` relevantStates && new `notElem` relevantStates
        then old
        else new

-- | Return True if the given PR is merged.
isPRMerged :: MonadMergeBot m => Int -> m Bool
isPRMerged prNum = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.pullRequest!.merged |] <$>
    runQuery PRIsMerged.query PRIsMerged.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _prNum = prNum
      }

-- | Get all queued PRs, by base branch.
getQueues :: MonadMergeBot m => m (HashMap Text [(Int, GitObjectID)])
getQueues  = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  fmap (HashMap.fromListWith (++)) $ queryAll_ $ \after -> do
    result <- runQuery QueuedPRs.query QueuedPRs.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _after = after
      , _appId = appId
      , _checkName = Text.unpack checkRunMerge
      }
    let payload = [get| result.repository!.pullRequests! |]
        info = [get| payload.pageInfo |]
    return PaginatedResult
      { payload = ()
      , chunk = mapMaybe getQueuedPR [get| payload.nodes![]! |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }
  where
    getQueuedPR pr = case concat [get| pr.headRef!.target.checkSuites!.nodes![]!.checkRuns!.nodes! |] of
      [] -> Nothing -- PR has no merge check run in the "queued" state
      _ -> Just
        ( [get| pr.baseRefName |]
        , [ [get| pr.(number, headRefOid) |] ]
        )

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
-- https://developer.github.com/v3/checks/runs/#update-a-check-run
updateCheckRun :: MonadMergeBot m => CheckRunId -> GitHubData -> m ()
updateCheckRun checkRunId ghData = void $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/check-runs/:check_run_id"
  , endpointVals = ["check_run_id" := checkRunId]
  , ghData
  }

-- | Create a commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadMergeBot m => Text -> GitObjectID -> [GitObjectID] -> m GitObjectID
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
createBranch :: MonadMergeBot m => Text -> GitObjectID -> m ()
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
updateBranch :: MonadMergeBot m => Bool -> Text -> GitObjectID -> m Bool
updateBranch force branch sha = fmap isRight $ githubTry $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/git/refs/:ref"
  , endpointVals = ["ref" := "heads/" <> branch]
  , ghData = ["sha" := sha, "force" := force]
  }

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
-- https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: MonadMergeBot m => Text -> m ()
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
mergeBranches :: MonadMergeBot m => Text -> GitObjectID -> Text -> m Bool
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
closePR :: MonadMergeBot m => Int -> m ()
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
  => (Maybe String -> m (PaginatedResult payload a)) -> m (payload, [a])
queryAll doQuery = queryAll' Nothing
  where
    queryAll' cursor = do
      result@PaginatedResult{..} <- doQuery cursor
      (_, next) <- case (hasNext, nextCursor) of
        (True, Just nextCursor') -> queryAll' . Just . Text.unpack $ nextCursor'
        (True, Nothing) -> fail $ "Paginated result says it has next with no cursor: " ++ show result
        (False, _) -> return (payload, [])
      return (payload, chunk ++ next)

queryAll_ :: (Monad m, Show a)
  => (Maybe String -> m (PaginatedResult () a)) -> m [a]
queryAll_ = fmap snd . queryAll
