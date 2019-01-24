{-|
Module      :  MergeBot.Core.Branch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions to query and manage branches utilized by the merge bot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.Branch
  ( getBranchStatuses
  , getTryStatus
  , getStagingStatus
  , createTryBranch
  , deleteTryBranch
  , createMergeBranch
  , getStagingPRs
  , mergeStaging
  ) where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Reader (asks)
import Data.Functor ((<&>))
import Data.GraphQL (get, runQuery, unwrap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Yaml (decodeThrow)

import MergeBot.Core.Branch.Internal
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
    , PaginatedResult(..)
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
import MergeBot.Core.GraphQL.Scalars.GitObjectID (GitObjectID(..))
import MergeBot.Core.Monad (MonadGraphQL, MonadREST, getRepo)

{- Branch operations -}

type Branch = [unwrap| (Branch.Schema).repository.ref!.target |]

-- | Get the given branch.
getBranch :: MonadGraphQL m => Text -> m Branch
getBranch = fmap fromJust . getBranch'

-- | Get the given branch.
getBranch' :: MonadGraphQL m => Text -> m (Maybe Branch)
getBranch' name = do
  (_repoOwner, _repoName) <- asks getRepo
  [get| .repository.ref?.target |] <$>
    runQuery Branch.query Branch.Args{_name = Text.unpack name, ..}

-- | Get all branches managed by the merge bot and the CI status of each.
getBranchStatuses :: MonadGraphQL m
  => [PullRequestId] -> m (Map PullRequestId BotStatus)
getBranchStatuses mergeQueue = do
  (_repoOwner, _repoName) <- asks getRepo
  branches <- queryAll $ \_after -> queryBranches Branches.Args{..}

  tryingPRs <- mapMaybeM parseTrying branches
  let queuedPRs = map (, MergeQueue) mergeQueue
  stagingPRs <- mapMaybeM parseStaging branches
  return $ Map.fromList $ tryingPRs ++ queuedPRs ++ concat stagingPRs
  where
    queryBranches args = do
      info <- [get| .repository.refs! |] <$> runQuery Branches.query args
      return PaginatedResult
        { chunk      = [get| info.nodes![]! |]
        , hasNext    = [get| info.pageInfo.hasNextPage |]
        , nextCursor = [get| info.pageInfo.endCursor |]
        }
    parseTrying branch =
      case fromTryBranch [get| branch.name |] of
        Just prNum -> do
          ciStatus <- getBranchStatus branch
          let status
                | isPending ciStatus = TryRunning
                | isSuccess ciStatus = TrySuccess
                | otherwise = TryFailed
          return $ Just (prNum, Trying status)
        Nothing -> return Nothing
    parseStaging branch =
      if isStagingBranch [get| branch.name |]
        then do
          ciStatus <- getBranchStatus branch
          let prIds = fromStagingMessage [get| branch.target.message! |]
              status = if isPending ciStatus || isSuccess ciStatus
                then MergeRunning else MergeFailed
          return $ Just $ map (, Merging status) prIds
        else return Nothing
    getBranchStatus branch = do
      let name = [get| branch.name |]
          invalid = fail $ "Branch does not have valid .lymerge.yaml: " ++ Text.unpack name
      config <- maybe invalid return . extractBranchConfig =<< getBranch name
      return $ toCIStatus config $ getContexts branch
    getContexts branch =
      let contexts = fromMaybe [] [get| branch.target.status?.contexts[] |]
      in map [get| .(context, state) |] contexts

-- | Get the CI status for the given branch.
getCIStatus :: MonadGraphQL m => Text -> m (Maybe CIStatus)
getCIStatus name = do
  (_repoOwner, _repoName) <- asks getRepo
  let getStatus branch = case extractBranchConfig branch of
        Nothing -> error $ "Branch does not have .lymerge.yaml: " ++ Text.unpack name
        Just config -> toCIStatus config $
          let contexts = [get| branch.status!.contexts[] |]
          in map [get| .(context, state) |] contexts
  fmap getStatus . [get| .repository.ref?.target |] <$>
    runQuery Branch.query Branch.Args{_name = Text.unpack name, ..}

-- | Get the CI status for the trying branch for the given PR.
getTryStatus :: MonadGraphQL m => PullRequestId -> m (Maybe CIStatus)
getTryStatus = getCIStatus . toTryBranch

-- | Get the CI status for the staging branch for the given base branch.
getStagingStatus :: MonadGraphQL m => Text -> m (Maybe CIStatus)
getStagingStatus = getCIStatus . toStagingBranch

-- | Create a CI branch by merging the given PRs on top of the given base branch.
createCIBranch :: (MonadGraphQL m, MonadREST m)
  => Text -> [PullRequestId] -> Text -> Text -> Text -> m ()
createCIBranch baseRef prs tempBranchName ciBranchName commitMessage = do
  -- delete temp/ci branches if they exist
  deleteBranch tempBranchName
  deleteBranch ciBranchName

  -- get base branch
  base <- getBranch baseRef
  let baseCommit = unOID [get| base.oid |]

  -- get PR commit hash
  prCommits <- mapM getPRCommit prs

  -- create a new temp commit off base
  tempCommit <- createCommit
    [ "message" := "[ci skip] temp"
    , "tree" := unOID [get| base.tree!.oid |]
    , "parents" :=* [baseCommit]
    ]

  -- create temp branch on new commit
  createBranch
    [ "ref" := "refs/heads/" <> tempBranchName
    , "sha" := tempCommit
    ]

  -- merge prs into temp branch and get the final git tree
  -- TODO: handle merge conflict
  mapM_ (mergeBranches . makeMerge tempBranchName) prCommits
  tempBranch <- getBranch tempBranchName

  case extractBranchConfig tempBranch of
    Nothing -> do
      deleteBranch tempBranchName
      fail "Missing or invalid .lymerge.yaml file"
    Just _ -> do
      -- create a new commit off the base branch
      ciCommit <- createCommit
        [ "message" := commitMessage
        , "tree" := unOID [get| tempBranch.tree!.oid |]
        , "parents" :=* (baseCommit : prCommits)
        ]

      -- create try branch on new commit
      createBranch
        [ "ref" := "refs/heads/" <> ciBranchName
        , "sha" := ciCommit
        ]

      -- delete temp branch
      deleteBranch tempBranchName

-- | Create a trying branch for the given PR.
createTryBranch :: (MonadGraphQL m, MonadREST m) => Text -> PullRequestId -> m ()
createTryBranch base prNum = createCIBranch base [prNum] tempBranchName tryBranch tryMessage
  where
    tryBranch = toTryBranch prNum
    tempBranchName = "temp-" <> tryBranch
    tryMessage = toTryMessage prNum

-- | Delete the trying branch for the given PR.
deleteTryBranch :: MonadREST m => PullRequestId -> m ()
deleteTryBranch = deleteBranch . toTryBranch

-- | Create a merge branch for the given PRs.
createMergeBranch :: (MonadGraphQL m, MonadREST m) => Text -> [PullRequestId] -> m ()
createMergeBranch base prs = createCIBranch base prs tempBranchName stagingBranch stagingMessage
  where
    stagingBranch = toStagingBranch base
    tempBranchName = "temp-" <> stagingBranch
    stagingMessage = toStagingMessage prs

-- | Get the pull requests currently in staging for the given base branch.
getStagingPRs :: MonadGraphQL m => Text -> m [PullRequestId]
getStagingPRs base = getBranch' (toStagingBranch base) <&> \case
  Nothing -> []
  Just branch -> fromStagingMessage [get| branch.message! |]

-- | Merge the staging branch into the base branch. Return Nothing if the merge fails and the list
-- of PRs merged otherwise.
mergeStaging :: (MonadGraphQL m, MonadREST m) => Text -> m (Maybe [PullRequestId])
mergeStaging base = do
  branch <- getBranch stagingBranch
  success <- updateBranch base ["sha" := unOID [get| branch.oid |]]
  if success
    then do
      deleteBranch stagingBranch
      return $ Just $ fromStagingMessage [get| branch.message! |]
    else return Nothing
  where
    stagingBranch = toStagingBranch base

{- Helpers -}

-- | Get the configuration file for the given branch.
extractBranchConfig :: Branch -> Maybe BranchConfig
extractBranchConfig branch =
  case filter ((== configFile) . [get| .name |]) entries of
    [] -> Nothing
    [entry] -> decodeThrow $ Text.encodeUtf8 [get| entry.object!.text! |]
    _ -> error "Multiple .lymerge.yaml files found?"
  where
    entries = [get| branch.tree!.entries![] |]
    configFile = ".lymerge.yaml"

-- | Get the commit hash for the given pull request.
getPRCommit :: MonadGraphQL m => Int -> m Text
getPRCommit prNum = do
  (_repoOwner, _repoName) <- asks getRepo
  unOID . [get| .repository.pullRequest!.headRefOid |] <$>
    runQuery PullRequest.query PullRequest.Args{_number=prNum, ..}

-- | Make a merge request payload for the given branch and commit hash.
makeMerge :: Text -> Text -> GitHubData
makeMerge branch commit =
  [ "base" := branch
  , "head" := commit
  , "message" := "[ci skip] merge into temp"
  ]
