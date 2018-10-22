{-|
Module      :  MergeBot.Core.Branch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions to query and manage branches utilized by the merge bot.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module MergeBot.Core.Branch
  ( getBranchStatuses
  ) where

import Data.GraphQL (MonadQuery, runQuery)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Text.Read (readMaybe)

import MergeBot.Core.Data (BotStatus(..), PullRequestId, TryStatus(..))
import MergeBot.Core.GitHub (queryAll)
import qualified MergeBot.Core.GraphQL.Branches as Branches
import MergeBot.Core.GraphQL.Enums (StatusState(..))
import MergeBot.Core.State (BotState, getMergeQueue, getRepo)

-- | Get all branches managed by the merge bot and the CI status of each.
getBranchStatuses :: MonadQuery m => BotState -> m (Map PullRequestId BotStatus)
getBranchStatuses state = do
  branches <- queryAll queryBranches
  let isStaging branch = [Branches.get| @branch.name |] == "staging"
      stagingPRs = case filter isStaging branches of
        [] -> []
        [_] -> [] -- TODO: get PRs in staging branch and get the status of the merge run
        _ -> error "Found multiple branches named staging?"
      tryingPRs = mapMaybe parseTrying branches
  return $ Map.fromList $ concat [stagingPRs, queuedPRs, tryingPRs]
  where
    (_repoOwner, _repoName) = getRepo state
    queryBranches _after = do
      result <- runQuery Branches.query Branches.Args{..}
      let info = [Branches.get| result.repository.refs! > info |]
      return
        ( [Branches.get| @info.nodes![]! > branch |]
        , [Branches.get| @info.pageInfo.hasNextPage |]
        , [Branches.get| @info.pageInfo.endCursor |]
        )
    queuedPRs = map (, MergeQueue) . Set.toList . getMergeQueue $ state
    parseTrying branch =
      let isTrying = Text.stripPrefix "trying-" [Branches.get| @branch.name |]
          maybeBranchId = readMaybe . Text.unpack =<< isTrying
          contexts = fromMaybe [] [Branches.get| @branch.target.status.contexts[].state |]
          status
            | any (`elem` [StatusStateEXPECTED, StatusStatePENDING]) contexts = TryRunning
            | all (== StatusStateSUCCESS) contexts = TrySuccess
            | otherwise = TryFailed
      in (, Trying status) <$> maybeBranchId
