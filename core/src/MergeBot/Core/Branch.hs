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
  ( toTryBranch
  , getBranchStatuses
  , getRequiredStatuses
  , getTryStatus
  , createTryBranch
  , createMergeBranch
  , mergeStaging
  ) where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (Object)
import Data.GraphQL (MonadQuery, runQuery)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Yaml (decodeThrow)
import Text.Read (readMaybe)

import MergeBot.Core.CIStatus (isPending, isSuccess, toCIStatus)
import MergeBot.Core.Config (BranchConfig(..))
import MergeBot.Core.Data
    ( BotStatus(..)
    , CIStatus(..)
    , MergeStatus(..)
    , PullRequestId
    , TryStatus(..)
    )
import MergeBot.Core.GitHub
    ( GitHubData
    , KeyValue(..)
    , MonadGitHub(..)
    , createBranch
    , createCommit
    , deleteBranch
    , mergeBranches
    , queryAll
    , updateBranch
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

-- | Get the commit hash and the tree SHA for the given branch.
getBranch :: (MonadReader BotEnv m, MonadQuery m) => Text -> m Object
getBranch name = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery Branch.query Branch.Args{_name = Text.unpack name, ..}
  return [Branch.get| result.repository.ref!.target > branch |]

-- | Get all branches managed by the merge bot and the CI status of each.
getBranchStatuses :: (MonadReader BotEnv m, MonadQuery m)
  => BotState -> m (Map PullRequestId BotStatus)
getBranchStatuses state = do
  (_repoOwner, _repoName) <- asks getRepo
  branches <- queryAll $ \_after -> queryBranches Branches.Args{..}
  let isStaging branch = [Branches.get| @branch.name |] == stagingBranch
  stagingPRs <- case filter isStaging branches of
    [] -> return []
    [branch] -> do
      staging <- getBranch stagingBranch
      config <- fromMaybe (error "Staging branch does not have .lymerge.yaml") . extractBranchConfig <$> getBranch stagingBranch
      let prIds = fromStagingMessage [Branch.get| @branch staging.message! |]
          ciStatus = toCIStatus config $ getContexts branch
          status = if isPending ciStatus || isSuccess ciStatus
            then MergeRunning else MergeFailed
      return $ map (, Merging status) prIds
    _ -> fail "Found multiple branches named staging?"
  tryingPRs <- mapMaybeM parseTrying branches
  return $ Map.fromList $ concat [tryingPRs, queuedPRs, stagingPRs]
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
    parseTrying branch = do
      let name = [Branches.get| @branch.name |]
      config <- fromMaybe (error "Trying branch does not have .lymerge.yaml") . extractBranchConfig <$> getBranch name
      let ciStatus = toCIStatus config $ getContexts branch
          status
            | isPending ciStatus = TryRunning
            | isSuccess ciStatus = TrySuccess
            | otherwise = TryFailed
      return $ (, Trying status) <$> fromTryBranch name
    getContexts branch =
      let contexts = fromMaybe [] [Branches.get| @branch.target.status.contexts[] > context |]
          fromContext context =
            ( [Branches.get| @context.context |]
            , [Branches.get| @context.state |]
            )
      in map fromContext contexts

-- | Get the CI statuses required to pass for the given branch.
getRequiredStatuses :: (MonadReader BotEnv m, MonadQuery m) => Text -> m [Text]
getRequiredStatuses = fmap (maybe [] requiredStatuses . extractBranchConfig) . getBranch

-- | Get the CI status for the trying branch for the given PR.
getTryStatus :: (MonadReader BotEnv m, MonadQuery m) => PullRequestId -> m (Maybe CIStatus)
getTryStatus prNum = do
  (_repoOwner, _repoName) <- asks getRepo
  result <- runQuery Branch.query Branch.Args{..}
  let ref = [Branch.get| result.repository.ref.target > branch |]
      getStatus branch = case extractBranchConfig branch of
        Nothing -> error "Try branch does not have .lymerge.yaml"
        Just config -> toCIStatus config $
          let contexts = [Branch.get| @branch.status!.contexts[] > context |]
              fromContext context =
                ( [Branch.get| @context.context |]
                , [Branch.get| @context.state |]
                )
          in map fromContext contexts
  return $ getStatus <$> ref
  where
    _name = Text.unpack $ toTryBranch prNum

-- | Create a trying branch for the given PR.
createTryBranch :: (MonadCatch m, MonadGitHub m, MonadReader BotEnv m, MonadQuery m)
  => PullRequestId -> m ()
createTryBranch prNum = do
  -- delete try branch if it exists
  deleteBranch tempBranchName
  deleteBranch tryBranch

  -- get master branch
  master <- getBranch "master"
  let masterCommit = [Branch.get| @branch master.oid |]
      masterTree = [Branch.get| @branch master.tree!.oid |]

  -- TODO: check that either master or PR branch has a config

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
    [ "ref" := "refs/heads/" <> tempBranchName
    , "sha" := tempCommit
    ]

  -- merge pr into temp branch
  -- TODO: handle merge conflict
  mergeBranches $ makeMerge tempBranchName prCommit
  tempBranch <- getBranch tempBranchName
  let mergeTree = [Branch.get| @branch tempBranch.tree!.oid |]

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
  deleteBranch tempBranchName
  where
    tempBranchName = "temp-" <> tryBranch
    tryBranch = toTryBranch prNum

-- | Create a merge branch for the given PRs.
createMergeBranch :: (MonadCatch m, MonadGitHub m, MonadReader BotEnv m, MonadQuery m)
  => [PullRequestId] -> m ()
createMergeBranch prs = do
  -- delete staging branch if it exists
  deleteBranch tempBranchName
  deleteBranch stagingBranch

  -- get master branch
  master <- getBranch "master"
  let masterCommit = [Branch.get| @branch master.oid |]
      masterTree = [Branch.get| @branch master.tree!.oid |]

  -- TODO: check that either master or one of PR branches has a config

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
    [ "ref" := "refs/heads/" <> tempBranchName
    , "sha" := tempCommit
    ]

  -- merge PRs into temp branch
  -- TODO: handle merge conflict
  mapM_ (mergeBranches . makeMerge tempBranchName) prCommits
  tempBranch <- getBranch tempBranchName
  let mergeTree = [Branch.get| @branch tempBranch.tree!.oid |]

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
  deleteBranch tempBranchName
  where
    tempBranchName = "temp-" <> stagingBranch

-- | Merge the staging branch into master. Return Nothing if the merge fails and the list of PRs
-- merged otherwise.
mergeStaging :: (MonadCatch m, MonadGitHub m, MonadReader BotEnv m, MonadQuery m)
  => m (Maybe [PullRequestId])
mergeStaging = do
  branch <- getBranch stagingBranch
  let commit = [Branch.get| @branch.oid |]
  success <- updateBranch "master" ["sha" := commit]
  if success
    then do
      deleteBranch stagingBranch
      return $ Just $ fromStagingMessage [Branch.get| @branch.message! |]
    else return Nothing

{- Helpers -}

-- | Get the configuration file for the given branch.
extractBranchConfig :: Object -> Maybe BranchConfig
extractBranchConfig branch =
  case filter isConfig entries of
    [] -> Nothing
    [entry] -> decodeThrow $ Text.encodeUtf8 [Branch.get| @entry.object!.text! |]
    _ -> error "Multiple .lymerge.yaml files found?"
  where
    entries = [Branch.get| @branch.tree!.entries![] > entry |]
    isConfig entry = [Branch.get| @entry.name |] == configFile
    configFile = ".lymerge.yaml"

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
