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
  , getMergeJobs
  , getPatchOptions
  -- Mutations
  , insertMergeQueue
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

import MergeBot.Error (BotError(..))
import MergeBot.Patch (PatchId, PatchOptionsPartial)

-- | The state of the merge bot.
data BotState = BotState
  { mergeQueue   :: Set PatchId
  , mergeJobs    :: Set PatchId
  , tryJobs      :: Set PatchId
  , patchOptions :: Map PatchId PatchOptionsPartial
    -- ^ Invariant: if PatchId is in mergeQueue or mergeJobs, it is in patchOptions
  } deriving (Show,Eq)

newBotState :: BotState
newBotState = BotState
  { mergeQueue = Set.empty
  , mergeJobs = Set.empty
  , tryJobs = Set.empty
  , patchOptions = Map.empty
  }

{- Querying state -}

-- | Helper for querying state.
inState :: (BotState -> Set PatchId) -> BotState -> PatchId -> Bool
inState f = flip Set.member . f

-- | Check if the given patch is in the merge queue.
inMergeQueue :: BotState -> PatchId -> Bool
inMergeQueue = inState mergeQueue

-- | Check if the given patch is a currently running merge job.
inMergeJobs :: BotState -> PatchId -> Bool
inMergeJobs = inState mergeJobs

-- | Check if the given patch is a currently running try job.
inTryJobs :: BotState -> PatchId -> Bool
inTryJobs = inState tryJobs

-- | Get all the patchs in the merge job list.
getMergeJobs :: BotState -> Set PatchId
getMergeJobs = mergeJobs

-- | Get the options for the given patch.
getPatchOptions :: BotState -> PatchId -> Maybe PatchOptionsPartial
getPatchOptions BotState{..} patch
  | all (patch `Set.notMember`) [mergeQueue, mergeJobs] = Nothing
  | otherwise = case Map.lookup patch patchOptions of
      Nothing -> fail $ "Patch in mergeJobs/mergeQueue does not have options: " ++ show patch
      Just options -> Just options

{- Modifying state -}

-- | Add the given patch to the merge queue.
--
-- Fails if the patch is already in the merge queue.
insertMergeQueue :: PatchId -> PatchOptionsPartial -> BotState -> Either BotError BotState
insertMergeQueue patch options state@BotState{..} = do
  when (patch `Set.member` mergeQueue) $ Left AlreadyInMergeQueue
  return $ state
    { mergeQueue = Set.insert patch mergeQueue
    , patchOptions = Map.insert patch options patchOptions
    }

-- | Remove the given patch from the merge queue.
--
-- Fails if the patch is not in the merge queue or is already running as a job.
removeMergeQueue :: PatchId -> BotState -> Either BotError BotState
removeMergeQueue patch state@BotState{..} = do
  when (patch `Set.member` mergeJobs) $ Left MergeJobStarted
  unless (patch `Set.member` mergeQueue) $ Left DoesNotExist
  return $ state
    { mergeQueue = Set.delete patch mergeQueue
    , patchOptions = Map.delete patch patchOptions
    }

-- | Clear the merge jobs, either after a successful merge or after cancelling the merge job.
clearMergeJobs :: BotState -> BotState
clearMergeJobs state@BotState{..} = state
  { mergeJobs = Set.empty
  , patchOptions = Map.withoutKeys patchOptions mergeJobs
  }

-- | Initialize a merge job with all the patchs in the queue.
initMergeJob :: BotState -> BotState
initMergeJob state@BotState{..} = state
  { mergeJobs = mergeQueue
  , mergeQueue = Set.empty
  }

-- | Start a try job for the given patch.
startTryJob :: PatchId -> BotState -> Either BotError BotState
startTryJob patch state@BotState{..} = do
  when (patch `Set.member` tryJobs) $ Left TryJobStarted
  return $ state { tryJobs = Set.insert patch tryJobs }
