{-|
Module      :  MergeBot.Core.Branch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions to query and manage branches utilized by the merge bot.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module MergeBot.Core.Branch
  ( getBranchStatuses
  , getTryStatus
  , createTryBranch
  , createMergeBranch
  ) where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Reader (MonadReader, asks)
import Data.GraphQL (MonadQuery, runQuery)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import MergeBot.Core.CIStatus (isPending, isSuccess, toCIStatus)
import MergeBot.Core.Data
    (BotStatus(..), CIStatus(..), PullRequestId, TryStatus(..))
import MergeBot.Core.GitHub
    ( GitHubData
    , KeyValue(..)
    , MonadGitHub(..)
    , createBranch
    , createCommit
    , deleteBranch
    , mergeBranches
    , queryAll
    )
import qualified MergeBot.Core.GraphQL.Branch as Branch
import qualified MergeBot.Core.GraphQL.Branches as Branches
import qualified MergeBot.Core.GraphQL.PullRequest as PullRequest
import MergeBot.Core.Monad (BotEnv, getRepo)
import MergeBot.Core.State (BotState, getMergeQueue)

{- Branch labels and messages -}

-- | Display the pull request number.
toId :: PullRequestId -> Text
toId = Text.pack . ('#':) . show

-- | Get the name of the try branch for the given pull request.
toTryBranch :: PullRequestId -> Text
toTryBranch = ("trying-" <>) . Text.pack . show

-- | Get the pull request for the given try branch.
fromTryBranch :: Text -> Maybe PullRequestId
fromTryBranch = readMaybe . Text.unpack <=< Text.stripPrefix "trying-"

-- | Get the try commit message for the given pull request.
toTryMessage :: PullRequestId -> Text
toTryMessage prNum = Text.unwords ["Try", toId prNum]

-- | Get the name of the staging branch.
stagingBranch :: Text
stagingBranch = "staging"

-- | Get the message for the staging branch.
toStagingMessage :: [PullRequestId] -> Text
toStagingMessage = Text.unwords . ("Merge":) . map toId

-- | Get the pull requests from the given message.
fromStagingMessage :: Text -> [PullRequestId]
fromStagingMessage = map (read . tail . Text.unpack) . tail . Text.words

{- Branch operations -}

-- | Get all branches managed by the merge bot and the CI status of each.
getBranchStatuses :: (MonadReader BotEnv m, MonadQuery m)
  => BotState -> m (Map PullRequestId BotStatus)
getBranchStatuses state = do
  (_repoOwner, _repoName) <- asks getRepo
  branches <- queryAll $ \_after -> queryBranches Branches.Args{..}
  let isStaging branch = [Branches.get| @branch.name |] == "staging"
      stagingPRs = case filter isStaging branches of
        [] -> []
        [_] -> [] -- TODO: get PRs in staging branch and get the status of the merge run
        _ -> error "Found multiple branches named staging?"
      tryingPRs = mapMaybe parseTrying branches
  return $ Map.fromList $ concat [stagingPRs, queuedPRs, tryingPRs]
  where
    queuedPRs = map (, MergeQueue) . Set.toList . getMergeQueue $ state
    queryBranches args = do
      result <- runQuery Branches.query args
      let info = [Branches.get| result.repository.refs! > info |]
      return
        ( [Branches.get| @info.nodes![]! > branch |]
        , [Branches.get| @info.pageInfo.hasNextPage |]
        , [Branches.get| @info.pageInfo.endCursor |]
        )
    parseTrying branch =
      let contexts = fromMaybe [] [Branches.get| @branch.target.status.contexts[] > context |]
          fromContext context =
            ( [Branches.get| @context.context |]
            , [Branches.get| @context.state |]
            )
          ciStatus = toCIStatus $ map fromContext contexts
          status
            | isPending ciStatus = TryRunning
            | isSuccess ciStatus = TrySuccess
            | otherwise = TryFailed
      in (, Trying status) <$> fromTryBranch [Branches.get| @branch.name |]

-- | Get the CI status for the trying branch for the given PR.
getTryStatus :: (MonadReader BotEnv m, MonadQuery m) => PullRequestId -> m (Maybe CIStatus)
getTryStatus prNum = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery Branch.query Branch.Args{..}
  case [Branch.get| result.repository.ref > ref |] of
    Nothing -> return Nothing
    Just ref -> return $ Just $ toCIStatus $
      let contexts = [Branch.get| @ref.target.status!.contexts[] > context |]
          fromContext context =
            ( [Branch.get| @context.context |]
            , [Branch.get| @context.state |]
            )
      in map fromContext contexts
  where
    _name = Text.unpack $ toTryBranch prNum

-- | Create a trying branch for the given PR.
createTryBranch :: (MonadCatch m, MonadGitHub m, MonadReader BotEnv m, MonadQuery m)
  => PullRequestId -> m ()
createTryBranch prNum = do
  -- delete try branch if it exists
  deleteBranch tempBranch
  deleteBranch tryBranch

  -- get master branch
  (masterCommit, masterTree) <- getBranch "master"

  -- get PR commit hash
  prCommit <- getPullRequest prNum

  -- create a new temp commit off master
  tempCommit <- createCommit
    [ "message" := "[ci skip] temp"
    , "tree" := masterTree
    , "parents" :=* [masterCommit]
    ]

  -- create temp branch on new commit
  createBranch
    [ "ref" := "refs/heads/" <> tempBranch
    , "sha" := tempCommit
    ]

  -- merge pr into temp branch
  -- TODO: handle merge conflict
  mergeBranches $ makeMerge tempBranch prCommit
  (_, mergeTree) <- getBranch tempBranch

  -- create a new try commit off master
  tryCommit <- createCommit
    [ "message" := toTryMessage prNum
    , "tree" := mergeTree
    , "parents" :=* [masterCommit, prCommit]
    ]

  -- create try branch on new commit
  createBranch
    [ "ref" := "refs/heads/" <> tryBranch
    , "sha" := tryCommit
    ]

  -- delete temp branch
  deleteBranch tempBranch
  where
    tempBranch = "temp-" <> tryBranch
    tryBranch = toTryBranch prNum

-- | Create a merge branch for the given PRs.
createMergeBranch :: (MonadCatch m, MonadGitHub m, MonadReader BotEnv m, MonadQuery m)
  => [PullRequestId] -> m ()
createMergeBranch prs = do
  -- delete staging branch if it exists
  deleteBranch tempBranch
  deleteBranch stagingBranch

  -- get master branch
  (masterCommit, masterTree) <- getBranch "master"

  -- get commit hash of PRs
  prCommits <- mapM getPullRequest prs

  -- create a new temp commit off master
  tempCommit <- createCommit
    [ "message" := "[ci skip] temp"
    , "tree" := masterTree
    , "parents" :=* [masterCommit]
    ]

  -- create temp branch on new commit
  createBranch
    [ "ref" := "refs/heads/" <> tempBranch
    , "sha" := tempCommit
    ]

  -- merge PRs into temp branch
  -- TODO: handle merge conflict
  mapM_ (mergeBranches . makeMerge tempBranch) prCommits
  (_, mergeTree) <- getBranch tempBranch

  -- create a new commit off master
  tryCommit <- createCommit
    [ "message" := toStagingMessage prs
    , "tree" := mergeTree
    , "parents" :=* (masterCommit : prCommits)
    ]

  -- create staging branch on new commit
  createBranch
    [ "ref" := "refs/heads/" <> stagingBranch
    , "sha" := tryCommit
    ]

  -- delete temp branch
  deleteBranch tempBranch
  where
    tempBranch = "temp-" <> stagingBranch

{- Helpers -}

-- | Get the commit hash and the tree SHA for the given branch.
getBranch :: (MonadReader BotEnv m, MonadQuery m) => Text -> m (Text, Text)
getBranch name = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery Branch.query Branch.Args{_name = Text.unpack name, ..}
  let branch = [Branch.get| result.repository.ref!.target > branch |]
      branchCommit = [Branch.get| @branch.oid |]
      branchTree = [Branch.get| @branch.tree!.oid |]
  return (branchCommit, branchTree)

-- | Get the commit hash for the given pull request.
getPullRequest :: (MonadReader BotEnv m, MonadQuery m) => Int -> m Text
getPullRequest _number = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery PullRequest.query PullRequest.Args{..}
  let pr = [PullRequest.get| result.repository.pullRequest! > pr |]
      commit = [PullRequest.get| @pr.headRefOid |]
  return commit

-- | Make a merge request payload for the given branch and commit hash.
makeMerge :: Text -> Text -> GitHubData
makeMerge branch commit =
  [ "base" := "refs/heads/" <> branch
  , "head" := commit
  , "message" := "[ci skip] merge into temp"
  ]
