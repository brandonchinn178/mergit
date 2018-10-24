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
  ) where

import Control.Monad.Reader (MonadReader, asks)
import Data.GraphQL (MonadQuery, runQuery)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Read (readMaybe)

import MergeBot.Core.CIStatus (isPending, isSuccess, toCIStatus)
import MergeBot.Core.Config (BotConfig, getRepo)
import MergeBot.Core.Data
    (BotStatus(..), CIStatus(..), PullRequestId, TryStatus(..))
import MergeBot.Core.GitHub (queryAll)
import qualified MergeBot.Core.GraphQL.Branch as Branch
import qualified MergeBot.Core.GraphQL.Branches as Branches
import MergeBot.Core.State (BotState, getMergeQueue)

-- | Get all branches managed by the merge bot and the CI status of each.
getBranchStatuses :: (MonadReader BotConfig m, MonadQuery m)
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
      let isTrying = Text.stripPrefix "trying-" [Branches.get| @branch.name |]
          maybeBranchId = readMaybe . Text.unpack =<< isTrying
          contexts = fromMaybe [] [Branches.get| @branch.target.status.contexts[] > context |]
          fromContext context =
            ( [Branches.get| @context.context |]
            , [Branches.get| @context.state |]
            )
          ciStatus = toCIStatus $ map fromContext contexts
          status
            | isPending ciStatus = TryRunning
            | isSuccess ciStatus = TrySuccess
            | otherwise = TryFailed
      in (, Trying status) <$> maybeBranchId

-- | Get the CI status for the trying branch for the given PR.
getTryStatus :: (MonadReader BotConfig m, MonadQuery m) => PullRequestId -> m (Maybe CIStatus)
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
    _name = "trying-" ++ show prNum
