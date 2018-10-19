{-|
Module      :  MergeBot.Core.State
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the internal state of the merge bot.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.State
  ( MergeQueue
  , BotState
  , newBotState
  , getMergeQueue
  , getRepo
  , clearMergeQueue
  , insertMergeQueue
  , removeMergeQueue
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import MergeBot.Core.Data (PullRequestId)

type MergeQueue = Set PullRequestId

-- | The state of the merge bot.
--
-- Most of the merge bot should be stateless; everything is either retrieved from the GitHub API
-- or stored in-memory. Could also cache replies from the GitHub API, but we should NOT be storing
-- persisted data anywhere.
data BotState = BotState
  { repoOwner  :: String
  , repoName   :: String
  , mergeQueue :: MergeQueue
  } deriving (Show)

newBotState :: String -> String -> BotState
newBotState repoOwner repoName = BotState{..}
  where
    mergeQueue = Set.empty

getMergeQueue :: BotState -> MergeQueue
getMergeQueue = mergeQueue

getRepo :: BotState -> (String, String)
getRepo BotState{..} = (repoOwner, repoName)

clearMergeQueue :: BotState -> BotState
clearMergeQueue state = state{mergeQueue = Set.empty}

insertMergeQueue :: PullRequestId -> BotState -> BotState
insertMergeQueue pr state@BotState{..} = state{mergeQueue = Set.insert pr mergeQueue}

removeMergeQueue :: PullRequestId -> BotState -> BotState
removeMergeQueue pr state@BotState{..} = state{mergeQueue = Set.delete pr mergeQueue}
