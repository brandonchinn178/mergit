{-|
Module      :  MergeBot.Core.State
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the internal state of the merge bot.
-}

module MergeBot.Core.State
  ( MergeQueue
  , BotState
  , newBotState
  , getQueued
  , getMergeQueue
  , clearMergeQueue
  , insertMergeQueue
  , removeMergeQueue
  ) where

import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import MergeBot.Core.Data (PullRequestId)

-- | The merge queue consists of PRs to merge for the given base branch.
type MergeQueue = Map Text (Set PullRequestId)

-- | The state of the merge bot.
--
-- Most of the merge bot should be stateless; everything is either retrieved from the GitHub API
-- or stored in-memory. Could also cache replies from the GitHub API, but we should NOT be storing
-- persisted data anywhere.
data BotState = BotState
  { mergeQueue :: MergeQueue
  } deriving (Show)

newBotState :: BotState
newBotState = BotState Map.empty

-- | Get all queued pull requests.
getQueued :: BotState -> Set PullRequestId
getQueued = fold . mergeQueue

-- | Get the merge queue for the given base branch.
getMergeQueue :: Text -> BotState -> Set PullRequestId
getMergeQueue base = fromMaybe Set.empty . Map.lookup base . mergeQueue

clearMergeQueue :: Text -> BotState -> BotState
clearMergeQueue base = mapMergeQueue $ Map.delete base

insertMergeQueue :: PullRequestId -> Text -> BotState -> BotState
insertMergeQueue pr base = mapMergeQueue $ Map.alter (Just . insertPR) base
  where
    insertPR = maybe (Set.singleton pr) (Set.insert pr)

removeMergeQueue :: PullRequestId -> Text -> BotState -> BotState
removeMergeQueue pr base = mapMergeQueue $ Map.adjust (Set.delete pr) base

{- Helpers -}

mapMergeQueue :: (MergeQueue -> MergeQueue) -> BotState -> BotState
mapMergeQueue f state = state { mergeQueue = f $ mergeQueue state }
