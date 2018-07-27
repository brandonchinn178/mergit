{-|
Module      :  MergeBot.State
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the internal state of the merge bot.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.State
  ( BotState
  , newBotState
  , addMergeQueue
  , removeMergeQueue
  , clearMergeJobs
  , startMergeJob
  , startTryJob
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MergeBot.Diff (DiffId, DiffOption)
import MergeBot.Error (BotError(..))

-- | The state of the merge bot.
data BotState = BotState
  { mergeQueue  :: Set DiffId
  , mergeJobs   :: Set DiffId
  , tryJobs     :: Set DiffId
  , diffOptions :: Map DiffId (Set DiffOption)
  } deriving (Show,Eq)

newBotState :: BotState
newBotState = BotState
  { mergeQueue = Set.empty
  , mergeJobs = Set.empty
  , tryJobs = Set.empty
  , diffOptions = Map.empty
  }

{- Modifying state -}

-- | Add the given diff to the merge queue.
--
-- Fails if the diff is already in the merge queue.
addMergeQueue :: DiffId -> Set DiffOption -> BotState -> Either BotError BotState
addMergeQueue diff options state@BotState{..} = do
  when (diff `Set.member` mergeQueue) $ Left AlreadyInMergeQueue
  return $ state
    { mergeQueue = Set.insert diff mergeQueue
    , diffOptions = Map.insert diff options diffOptions -- TODO: clear this at some point
    }

-- | Remove the given diff from the merge queue.
--
-- Fails if the diff is not in the merge queue or is already running as a job.
removeMergeQueue :: DiffId -> BotState -> Either BotError BotState
removeMergeQueue diff state@BotState{..} = do
  when (diff `Set.member` mergeJobs) $ Left MergeJobStarted
  unless (diff `Set.member` mergeQueue) $ Left DoesNotExist
  return $ state
    { mergeQueue = Set.delete diff mergeQueue
    }

-- | Clear the merge jobs, either after a successful merge or after cancelling the merge job.
clearMergeJobs :: BotState -> BotState
clearMergeJobs state = state { mergeJobs = Set.empty }

-- | Start a merge job with all the diffs in the queue.
startMergeJob :: BotState -> BotState
startMergeJob state@BotState{..} = state
  { mergeJobs = mergeQueue
  , mergeQueue = Set.empty
  }

-- | Start a try job for the given diff.
startTryJob :: DiffId -> BotState -> Either BotError BotState
startTryJob diff state@BotState{..} = do
  when (diff `Set.member` tryJobs) $ Left TryJobStarted
  return $ state { tryJobs = Set.insert diff tryJobs }
