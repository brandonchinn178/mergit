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
  -- Queries
  , inMergeQueue
  , inMergeJobs
  , inTryJobs
  , getDiffOptions
  -- Mutations
  , addMergeQueue
  , removeMergeQueue
  , clearMergeJobs
  , initMergeJob
  , startTryJob
  ) where

import Control.Monad (unless, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import MergeBot.Diff (DiffId, DiffOptionsPartial)
import MergeBot.Error (BotError(..))

-- | The state of the merge bot.
data BotState = BotState
  { mergeQueue  :: Set DiffId
  , mergeJobs   :: Set DiffId
  , tryJobs     :: Set DiffId
  , diffOptions :: Map DiffId DiffOptionsPartial
  } deriving (Show,Eq)

newBotState :: BotState
newBotState = BotState
  { mergeQueue = Set.empty
  , mergeJobs = Set.empty
  , tryJobs = Set.empty
  , diffOptions = Map.empty
  }

{- Querying state -}

-- | Helper for querying state.
inState :: (BotState -> Set DiffId) -> BotState -> DiffId -> Bool
inState f = flip Set.member . f

-- | Check if the given diff is in the merge queue.
inMergeQueue :: BotState -> DiffId -> Bool
inMergeQueue = inState mergeQueue

-- | Check if the given diff is a currently running merge job.
inMergeJobs :: BotState -> DiffId -> Bool
inMergeJobs = inState mergeJobs

-- | Check if the given diff is a currently running try job.
inTryJobs :: BotState -> DiffId -> Bool
inTryJobs = inState tryJobs

-- | Get the options for the given diff.
getDiffOptions :: BotState -> DiffId -> Maybe DiffOptionsPartial
getDiffOptions = flip Map.lookup . diffOptions

{- Modifying state -}

-- | Add the given diff to the merge queue.
--
-- Fails if the diff is already in the merge queue.
addMergeQueue :: DiffId -> DiffOptionsPartial -> BotState -> Either BotError BotState
addMergeQueue diff options state@BotState{..} = do
  when (diff `Set.member` mergeQueue) $ Left AlreadyInMergeQueue
  return $ state
    { mergeQueue = Set.insert diff mergeQueue
    , diffOptions = Map.insert diff options diffOptions
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
    , diffOptions = Map.delete diff diffOptions
    }

-- | Clear the merge jobs, either after a successful merge or after cancelling the merge job.
clearMergeJobs :: BotState -> BotState
clearMergeJobs state@BotState{..} = state
  { mergeJobs = Set.empty
  , diffOptions = Map.withoutKeys diffOptions mergeJobs
  }

-- | Initialize a merge job with all the diffs in the queue.
initMergeJob :: BotState -> BotState
initMergeJob state@BotState{..} = state
  { mergeJobs = mergeQueue
  , mergeQueue = Set.empty
  }

-- | Start a try job for the given diff.
startTryJob :: DiffId -> BotState -> Either BotError BotState
startTryJob diff state@BotState{..} = do
  when (diff `Set.member` tryJobs) $ Left TryJobStarted
  return $ state { tryJobs = Set.insert diff tryJobs }
