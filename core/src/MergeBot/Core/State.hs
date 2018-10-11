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
  , clearMergeQueue
  , insertMergeQueue
  , removeMergeQueue
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import GitHub.Data.Id (Id)
import GitHub.Data.PullRequests (PullRequest)

type MergeQueue = Set (Id PullRequest)

-- | The state of the merge bot.
--
-- Most of the merge bot should be stateless; everything is either retrieved from the GitHub API
-- or stored in-memory. Could also cache replies from the GitHub API, but we should NOT be storing
-- persisted data anywhere.
newtype BotState = BotState
  { mergeQueue :: MergeQueue
  } deriving (Show)

newBotState :: BotState
newBotState = BotState Set.empty

getMergeQueue :: BotState -> MergeQueue
getMergeQueue = mergeQueue

clearMergeQueue :: BotState -> BotState
clearMergeQueue state = state{mergeQueue = Set.empty}

insertMergeQueue :: Id PullRequest -> BotState -> BotState
insertMergeQueue pr state@BotState{..} = state{mergeQueue = Set.insert pr mergeQueue}

removeMergeQueue :: Id PullRequest -> BotState -> BotState
removeMergeQueue pr state@BotState{..} = state{mergeQueue = Set.delete pr mergeQueue}
