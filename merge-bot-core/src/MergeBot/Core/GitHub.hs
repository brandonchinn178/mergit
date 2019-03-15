{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.GitHub
  ( -- * GraphQL
    getCIParents
  , getTree, Tree
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
import Data.Either (isRight)
import Data.GraphQL (get, runQuery, unwrap)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST
    (GHEndpoint(..), GitHubData, KeyValue(..), StdMethod(..), githubTry, (.:))

import qualified MergeBot.Core.GraphQL.BranchTree as BranchTree
import qualified MergeBot.Core.GraphQL.ParentsStatus as ParentsStatus
import MergeBot.Core.Monad (MonadMergeBot(..), queryGitHub')

{- GraphQL -}

-- | A commit's CI statuses and check runs.
type CommitStatus = [unwrap| (ParentsStatus.Schema).repository!.object!.parents!.nodes![]! |]

-- | Get the parent commits for the given CI commit.
getCIParents :: MonadMergeBot m => GitObjectID -> Text -> m [CommitStatus]
getCIParents sha checkName = do
  (repoOwner, repoName) <- getRepo
  appId <- getAppId
  queryAll $ \after -> do
    result <- runQuery ParentsStatus.query ParentsStatus.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _appId = appId
      , _after = after
      , _sha = sha
      , _checkName = Just $ Text.unpack checkName
      }
    let info = [get| result.repository!.object!.parents!.pageInfo |]
    return PaginatedResult
      { chunk = [get| result.repository!.object!.parents!.nodes![]! |]
      , hasNext = [get| info.hasNextPage |]
      , nextCursor = [get| info.endCursor |]
      }

type Tree = [unwrap| (BranchTree.Schema).repository.ref!.target.tree! |]

-- | Get the git tree for the given branch.
getTree :: MonadMergeBot m => Text -> m Tree
getTree branch = do
  (repoOwner, repoName) <- getRepo
  [get| .repository.ref!.target.tree! |] <$>
    runQuery BranchTree.query BranchTree.Args
      { _repoOwner = Text.unpack repoOwner
      , _repoName = Text.unpack repoName
      , _name = Text.unpack branch
      }

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
updateCheckRun :: MonadMergeBot m => Int -> GitHubData -> m ()
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

data PaginatedResult a = PaginatedResult
  { chunk      :: [a]
  , hasNext    :: Bool
  , nextCursor :: Maybe Text
  } deriving (Show)

-- | Run a paginated query as many times as possible until all the results have been fetched.
queryAll :: (Monad m, Show a) => (Maybe String -> m (PaginatedResult a)) -> m [a]
queryAll doQuery = queryAll' Nothing
  where
    queryAll' cursor = do
      result@PaginatedResult{..} <- doQuery cursor
      next <- case (hasNext, nextCursor) of
        (True, Just nextCursor') -> queryAll' . Just . Text.unpack $ nextCursor'
        (True, Nothing) -> fail $ "Paginated result says it has next with no cursor: " ++ show result
        (False, _) -> return []
      return $ chunk ++ next
