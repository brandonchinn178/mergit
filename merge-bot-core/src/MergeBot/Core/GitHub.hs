{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GitHub
  ( -- * GraphQL
    Tree
  , getBranchTree
  , CIContext
  , CICommit(..)
  , getCICommit
  , getQueues
  , getStagingAndSHA
    -- * REST
  , createCheckRun
  , updateCheckRun
  , createCommit
  , createBranch
  , updateBranch
  , deleteBranch
  , mergeBranches
  ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.GraphQL (get, mkGetter, runQuery, unwrap)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST
    (GHEndpoint(..), GitHubData, KeyValue(..), StdMethod(..), githubTry, (.:))

import qualified MergeBot.Core.GraphQL.BranchTree as BranchTree
import qualified MergeBot.Core.GraphQL.CICommit as CICommit
import qualified MergeBot.Core.GraphQL.GetBaseAndCIBranches as GetBaseAndCIBranches
import qualified MergeBot.Core.GraphQL.QueuedPRs as QueuedPRs
import MergeBot.Core.Monad (MonadMergeBot(..), queryGitHub')
import MergeBot.Core.Text (checkRunMerge, toStagingBranch)

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

type CIContext = [unwrap| (CICommit.Schema).repository!.object!.status!.contexts[] |]

data CICommit = CICommit
  { commitTree     :: Tree
  , commitContexts :: [CIContext]
  , checkRuns      :: [CheckRunId]
  }

-- | Get details for the given CI commit.
getCICommit :: MonadMergeBot m => GitObjectID -> Text -> m CICommit
getCICommit sha checkName = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  (result, checkRuns) <- queryAll $ \after -> do
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
        parents = [get| payload.parents!.nodes![]! |]
        checkSuites = concatMap [get| .checkSuites!.nodes![]! |] parents
        checkRuns = concatMap [get| .checkRuns!.nodes![]! |] checkSuites
    return PaginatedResult
      { payload
      , chunk = map [get| .databaseId |] checkRuns
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }
  return CICommit
    { commitTree = [get| result.tree! |]
    , commitContexts = fromMaybe [] [get| result.status?.contexts |]
    , checkRuns
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

-- | Get whether the staging branch exists for the given base branch and HEAD for the base branch.
getStagingAndSHA :: MonadMergeBot m => Text -> m (Bool, GitObjectID)
getStagingAndSHA baseBranch = do
  (repoOwner, repoName) <- getRepo
  result <- runQuery GetBaseAndCIBranches.query GetBaseAndCIBranches.Args
    { _repoOwner = Text.unpack repoOwner
    , _repoName = Text.unpack repoName
    , _baseBranch = Text.unpack baseBranch
    , _ciBranch = Text.unpack $ toStagingBranch baseBranch
    }
  return $ first isJust [get| result.repository!.(ciBranch, baseBranch!.target.oid) |]

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
mergeBranches base sha message = fmap isRight $ githubTry $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/merges"
  , endpointVals = []
  , ghData =
    [ "base"           := base
    , "head"           := sha
    , "commit_message" := message
    ]
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
