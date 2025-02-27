{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-|
Module      :  Mergit.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
module Mergit.Core.GitHub (
  -- * Types
  Repo,
  PrNum,
  CommitSHA,
  BranchName,
  UserName,
  CheckRunType (..),

  -- * GraphQL
  Tree,
  getBranchTree,
  getBranchSHA,
  CIContext,
  CICommit (..),
  getCICommit,
  CheckRunId,
  CheckRunInfo (..),
  getCheckRun,
  getCheckRunForCommit,
  PullRequest (..),
  getPRForCommit,
  getPRById,
  getPRReviews,
  isPRMerged,
  getQueues,

  -- * REST
  createCheckRun,
  updateCheckRun',
  createCommit,
  createBranch,
  updateBranch,
  deleteBranch,
  mergeBranches,
  closePR,
) where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Internal (LookupSchema, SchemaResult)
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.GraphQL (MonadGraphQLQuery, get, mkGetter, runQuery, unwrap)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (
  GHEndpoint (..),
  GitHubData,
  KeyValue (..),
  StdMethod (..),
  githubTry,
  githubTry',
  (.:),
 )
import Network.HTTP.Types (status409)
import Text.Printf (printf)
import UnliftIO.Exception (throwIO)

import Mergit.Core.Error (MergitError (..))
import Mergit.Core.GraphQL.API (
  GetBranchSHAQuery (..),
  GetBranchTreeQuery (..),
  GetBranchTreeSchema,
  GetCICommitQuery (..),
  GetCICommitSchema,
  GetCommitCheckRunQuery (..),
  GetIsPRMergedQuery (..),
  GetPRByIdQuery (..),
  GetPRCheckRunQuery (..),
  GetPRForCommitQuery (..),
  GetPRReviewsQuery (..),
  GetQueuedPRsQuery (..),
 )
import Mergit.Core.GraphQL.Enums.CheckConclusionState (CheckConclusionState)
import Mergit.Core.GraphQL.Enums.CheckStatusState (CheckStatusState)
import Mergit.Core.GraphQL.Enums.PullRequestReviewState (PullRequestReviewState)
import Mergit.Core.GraphQL.Enums.PullRequestReviewState qualified as PullRequestReviewState
import Mergit.Core.Monad (MonadMergit, MonadMergitEnv (..), queryGitHub')
import Mergit.Core.Text (checkRunMerge, checkRunTry, fromStagingMessage)

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
getBranchTree :: MonadMergit m => BranchName -> m Tree
getBranchTree branch = do
  (repoOwner, repoName) <- getRepo
  getTree
    <$> runQuery
      GetBranchTreeQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _name = branch
        }

-- | Get the git hash for the given branch, if it exists.
getBranchSHA :: MonadMergit m => BranchName -> m (Maybe CommitSHA)
getBranchSHA branch = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.ref?.target!.oid |]
    <$> runQuery
      GetBranchSHAQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _branch = branch
        }

type CIContext = [unwrap| GetCICommitSchema.repository!.object!.__fragment!.status!.contexts[] |]

data CICommit = CICommit
  { commitTree :: Tree
  , commitContexts :: [CIContext]
  , prsFromMessage :: [Int]
  -- ^ Pull request numbers parsed from the commit message. Not guaranteed to be in any
  -- order corresponding to the 'parents' list.
  , parents :: [(CommitSHA, CheckRunInfo)]
  -- ^ The parent commits of a CI commit, not including the base branch
  }
  deriving (Show)

-- | Get details for the given CI commit; that is, a commit created by 'createCIBranch'.
getCICommit ::
  ( MonadIO m
  , MonadGraphQLQuery m
  , MonadMergitEnv m
  ) =>
  CommitSHA
  -> CheckRunType
  -> m CICommit
getCICommit sha checkRunType = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  (result, parentsPayload) <- queryAll $ \after -> do
    result <-
      runQuery
        GetCICommitQuery
          { _repoOwner = repoOwner
          , _repoName = repoName
          , _appId = appId
          , _after = after
          , _sha = sha
          , _checkName = Just checkName
          }

    let payload = [get| result.repository!.object!.__fragment! |]
        info = [get| payload.parents.pageInfo |]

    pure
      PaginatedResult
        { payload
        , chunk = [get| payload.parents.nodes![]! |]
        , hasNext = [get| info.hasNextPage |]
        , nextCursor = [get| info.endCursor |]
        }

  parents <- forM
    (tail parentsPayload) -- ignore base branch, which is always first
    $ \parent -> do
      let parentSHA = [get| parent.oid |]
      case parseCommitCheckRunFragments parent of
        [] -> throwIO $ MissingCheckRun parentSHA checkName
        [checkRun] -> pure (parentSHA, checkRun)
        _ -> error $ printf "Commit has multiple check runs named '%s': %s" checkName (show parent)

  pure
    CICommit
      { commitTree = [get| result.tree |]
      , commitContexts = fromMaybe [] [get| result.status?.contexts |]
      , prsFromMessage = case fromStagingMessage [get| result.message |] of
          Just (_, prIds) -> prIds
          Nothing -> error $ printf "Could not parse CI commit message: %s" [get| result.message |]
      , parents
      }
  where
    checkName = getCheckName checkRunType

type CheckRunId = Int

data CheckRunInfo = CheckRunInfo
  { checkRunId :: CheckRunId
  , checkRunState :: CheckStatusState
  , checkRunConclusion :: Maybe CheckConclusionState
  }
  deriving (Show, Eq)

-- | Get the check run for the given PR and check run name.
getCheckRun :: MonadMergit m => PrNum -> CheckRunType -> m CheckRunInfo
getCheckRun prNum checkRunType = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  result <-
    runQuery
      GetPRCheckRunQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _prNum = prNum
        , _appId = appId
        , _checkName = checkName
        }
  commit <- case [get| result.repository!.pullRequest!.commits.nodes![]!.commit |] of
    [] -> error $ printf "PR #%d has no commits: %s" prNum (show result)
    [c] -> pure c
    _ -> error $ "PRCheckRun query returned more than one 'last' commit: " ++ show result
  case parseCommitCheckRunFragments commit of
    [checkRun] -> pure checkRun
    _ -> throwIO $ MissingCheckRunPR prNum checkName
  where
    checkName = getCheckName checkRunType

-- | Get the check run for the given commit and check run name.
getCheckRunForCommit :: MonadMergit m => CommitSHA -> CheckRunType -> m CheckRunInfo
getCheckRunForCommit sha checkRunType = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  result <-
    runQuery
      GetCommitCheckRunQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _sha = sha
        , _appId = appId
        , _checkName = checkName
        }
  let commit = [get| result.repository!.object!.__fragment! |]
  case parseCommitCheckRunFragments commit of
    [checkRun] -> pure checkRun
    _ -> throwIO $ MissingCheckRun sha checkName
  where
    checkName = getCheckName checkRunType

data PullRequest = PullRequest
  { prId :: Int
  , prBaseBranch :: Text
  , prSHA :: GitObjectID
  , prBranch :: Text
  , prIsMerged :: Bool
  }
  deriving (Show, Eq)

-- | Get information for the associated PR for the given commit.
--
--  If the commit is only associated with one PR, return it. Otherwise, find a single
--  PR with the given commit as its HEAD. If we still can't narrow down to a single PR,
--  throw an error.
getPRForCommit ::
  ( MonadIO m
  , MonadGraphQLQuery m
  , MonadMergitEnv m
  ) =>
  GitObjectID
  -> m PullRequest
getPRForCommit sha = do
  (repoOwner, repoName) <- getRepo

  prs <- queryAll_ $ \after -> do
    result <-
      runQuery
        GetPRForCommitQuery
          { _repoOwner = repoOwner
          , _repoName = repoName
          , _sha = sha
          , _after = after
          }
    let payload = [get| result.repository!.object!.__fragment!.associatedPullRequests! |]
    pure
      PaginatedResult
        { payload = ()
        , chunk = [get| payload.nodes![]! |]
        , hasNext = [get| payload.pageInfo.hasNextPage |]
        , nextCursor = [get| payload.pageInfo.endCursor |]
        }

  pr <-
    if
        | null prs -> throwIO $ CommitLacksPR sha
        | [pr] <- prs -> pure pr
        | [pr] <- filter ((== sha) . [get| .headRefOid |]) prs -> pure pr
        | otherwise -> throwIO $ AmbiguousPRForCommit sha

  pure
    PullRequest
      { prId = [get| pr.number |]
      , prBaseBranch = [get| pr.baseRefName |]
      , prSHA = [get| pr.headRefOid |]
      , prBranch = [get| pr.headRefName |]
      , prIsMerged = [get| pr.merged |]
      }

-- | Get information for the given PR.
getPRById :: MonadMergit m => Int -> m PullRequest
getPRById prId = do
  (repoOwner, repoName) <- getRepo

  result <-
    runQuery
      GetPRByIdQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _id = prId
        }

  let pr = [get| result.repository!.pullRequest! |]

  pure
    PullRequest
      { prId = [get| pr.number |]
      , prBaseBranch = [get| pr.baseRefName |]
      , prSHA = [get| pr.headRefOid |]
      , prBranch = [get| pr.headRefName |]
      , prIsMerged = [get| pr.merged |]
      }

-- | Return the reviews for the given PR as a map from reviewer to review state.
getPRReviews :: MonadMergit m => PrNum -> m (HashMap UserName PullRequestReviewState)
getPRReviews prNum = do
  (repoOwner, repoName) <- getRepo
  fmap (HashMap.fromListWith resolve) $
    queryAll_ $ \after -> do
      result <-
        runQuery
          GetPRReviewsQuery
            { _repoOwner = repoOwner
            , _repoName = repoName
            , _prNum = prNum
            , _after = after
            }
      let payload = [get| result.repository!.pullRequest!.reviews! |]
          info = [get| payload.pageInfo |]
      pure
        PaginatedResult
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
isPRMerged :: MonadMergit m => PrNum -> m Bool
isPRMerged prNum = do
  (repoOwner, repoName) <- getRepo
  [get| .repository!.pullRequest!.merged |]
    <$> runQuery
      GetIsPRMergedQuery
        { _repoOwner = repoOwner
        , _repoName = repoName
        , _prNum = prNum
        }

-- | Get all queued PRs, by base branch.
getQueues :: MonadMergit m => m (HashMap Text [(PrNum, CommitSHA, CheckRunInfo)])
getQueues = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  fmap (HashMap.fromListWith (++)) $
    queryAll_ $ \after -> do
      result <-
        runQuery
          GetQueuedPRsQuery
            { _repoOwner = repoOwner
            , _repoName = repoName
            , _after = after
            , _appId = appId
            , _checkName = checkRunMerge
            }
      let payload = [get| result.repository!.pullRequests |]
          info = [get| payload.pageInfo |]
      pure
        PaginatedResult
          { payload = ()
          , chunk = mapMaybe getQueuedPR [get| payload.nodes![]! |]
          , hasNext = [get| info.hasNextPage |]
          , nextCursor = [get| info.endCursor |]
          }
  where
    getQueuedPR pr =
      let prCommit = [get| pr.headRef!.target!.__fragment! |]
          (base, number, headRef) = [get| pr.(baseRefName, number, headRefOid) |]
       in case concat [get| prCommit.checkSuites!.nodes![]!.checkRuns!.nodes![]! |] of
            [] -> Nothing -- PR has no merge check run in the "queued" state
            checkRun : _ -> Just (base, [(number, headRef, parseCheckRunInfoFragment checkRun)])

{- REST -}

-- | Create a check run.
--
--  https://developer.github.com/v3/checks/runs/#create-a-check-run
createCheckRun :: MonadMergit m => GitHubData -> m ()
createCheckRun ghData =
  void $
    queryGitHub'
      GHEndpoint
        { method = POST
        , endpoint = "/repos/:owner/:repo/check-runs"
        , endpointVals = []
        , ghData
        }

-- | Update a check run.
--
--  NOTE: Should NOT be run directly. Use Mergit.Core.CheckRun.updateCheckRun instead.
--
--  https://developer.github.com/v3/checks/runs/#update-a-check-run
updateCheckRun' :: MonadMergit m => CheckRunId -> GitHubData -> m ()
updateCheckRun' checkRunId ghData = void $ queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
        { method = PATCH
        , endpoint = "/repos/:owner/:repo/check-runs/:check_run_id"
        , endpointVals = ["check_run_id" := checkRunId]
        , ghData
        }

-- | Create a commit.
--
--  https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadMergit m => Text -> GitObjectID -> [CommitSHA] -> m CommitSHA
createCommit message tree parents = (.: "sha") <$> queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
        { method = POST
        , endpoint = "/repos/:owner/:repo/git/commits"
        , endpointVals = []
        , ghData =
            [ "message" := message
            , "tree" := tree
            , "parents" := parents
            ]
        }

-- | Create a branch.
--
--  https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadMergit m => BranchName -> CommitSHA -> m ()
createBranch name sha = void $ queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
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
--  Returns False if update is not a fast-forward.
--
--  https://developer.github.com/v3/git/refs/#update-a-reference
updateBranch :: MonadMergit m => Bool -> BranchName -> CommitSHA -> m (Either Text ())
updateBranch force branch sha = fmap resolve $ githubTry $ queryGitHub' endpoint
  where
    resolve = bimap (.: "message") (const ())
    endpoint =
      GHEndpoint
        { method = PATCH
        , endpoint = "/repos/:owner/:repo/git/refs/:ref"
        , endpointVals = ["ref" := "heads/" <> branch]
        , ghData = ["sha" := sha, "force" := force]
        }

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
--  https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: MonadMergit m => BranchName -> m ()
deleteBranch branch = void $ githubTry $ queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
        { method = DELETE
        , endpoint = "/repos/:owner/:repo/git/refs/:ref"
        , endpointVals = ["ref" := "heads/" <> branch]
        , ghData = []
        }

-- | Merge two branches, returning the merge commit information.
--
--  Returns False if there was a merge conflict
--
--  https://developer.github.com/v3/repos/merging/#perform-a-merge
mergeBranches :: MonadMergit m => BranchName -> CommitSHA -> Text -> m Bool
mergeBranches base sha message = fmap isRight $ githubTry' status409 $ queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
        { method = POST
        , endpoint = "/repos/:owner/:repo/merges"
        , endpointVals = []
        , ghData =
            [ "base" := base
            , "head" := sha
            , "commit_message" := message
            ]
        }

-- | Close the given PR.
--
--  https://developer.github.com/v3/pulls/#update-a-pull-request
closePR :: MonadMergit m => PrNum -> m ()
closePR prNum = void $ queryGitHub' endpoint
  where
    endpoint =
      GHEndpoint
        { method = PATCH
        , endpoint = "/repos/:owner/:repo/pulls/:number"
        , endpointVals = ["number" := prNum]
        , ghData = ["state" := "closed"]
        }

{- Fragments -}

type CheckRunInfoFragmentSchema =
  [schema|
    {
      databaseId: Maybe Int,
      status: CheckStatusState,
      conclusion: Maybe CheckConclusionState,
    }
  |]

parseCheckRunInfoFragment :: Object CheckRunInfoFragmentSchema -> CheckRunInfo
parseCheckRunInfoFragment o =
  CheckRunInfo
    { checkRunId = [get| o.databaseId! |]
    , checkRunState = [get| o.status |]
    , checkRunConclusion = [get| o.conclusion |]
    }

-- https://github.com/LeapYear/aeson-schemas/issues/80
parseCommitCheckRunFragments ::
  ( s1 ~ LookupSchema "checkSuites" schema
  , Maybe (Object r1) ~ SchemaResult s1
  , s2 ~ LookupSchema "nodes" r1
  , Maybe [Maybe (Object r2)] ~ SchemaResult s2
  , s3 ~ LookupSchema "checkRuns" r2
  , Maybe (Object r3) ~ SchemaResult s3
  , s4 ~ LookupSchema "nodes" r3
  , Maybe [Maybe (Object CheckRunInfoFragmentSchema)] ~ SchemaResult s4
  , Typeable s1
  , Typeable s2
  , Typeable s3
  , Typeable s4
  , Typeable r1
  , Typeable r2
  , Typeable r3
  ) =>
  Object schema
  -> [CheckRunInfo]
parseCommitCheckRunFragments =
  map parseCheckRunInfoFragment . concat . [get| .checkSuites!.nodes![]!.checkRuns!.nodes![]! |]

{- Helpers -}

data PaginatedResult payload a = PaginatedResult
  { payload :: payload
  -- ^ The full payload of the first page
  , chunk :: [a]
  -- ^ The paginated part of the payload
  , hasNext :: Bool
  , nextCursor :: Maybe Text
  }
  deriving (Show)

-- | Run a paginated query as many times as possible until all the results have been fetched.
queryAll ::
  (Monad m, Show payload, Show a) =>
  (Maybe Text -> m (PaginatedResult payload a))
  -> m (payload, [a])
queryAll doQuery = queryAll' Nothing
  where
    queryAll' cursor = do
      result@PaginatedResult{..} <- doQuery cursor
      (_, next) <- case (hasNext, nextCursor) of
        (True, Just nextCursor') -> queryAll' $ Just nextCursor'
        (True, Nothing) -> error $ "Paginated result says it has next with no cursor: " ++ show result
        (False, _) -> pure (payload, [])
      pure (payload, chunk ++ next)

queryAll_ ::
  (Monad m, Show a) =>
  (Maybe Text -> m (PaginatedResult () a))
  -> m [a]
queryAll_ = fmap snd . queryAll
