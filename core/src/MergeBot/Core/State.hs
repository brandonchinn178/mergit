{-|
Module      :  MergeBot.Core.State
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data type representing the internal state of the merge bot.
-}

module MergeBot.Core.State
  ( BotState
  , newBotState
  -- Queries
  , getMergeJobs
  , getPatchOptions
  -- Mutations
  , insertMergeQueue
  , removeMergeQueue
  , clearMergeJobs
  , initMergeJob
  , startTryJob
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

import MergeBot.Core.Patch (PatchId, PatchOptionsPartial)

{- PatchState -}

data PatchState
  = TryJob
  | MergeQueue PatchOptionsPartial
  | MergeJob PatchOptionsPartial
  deriving (Show)

isMergeJob :: PatchState -> Bool
isMergeJob (MergeJob _) = True
isMergeJob _ = False

{- BotState -}

-- | The state of the merge bot.
newtype BotState = BotState (Map PatchId PatchState)
  deriving (Show)

newBotState :: BotState
newBotState = BotState Map.empty

{- Querying state -}

-- | Get all the patchs in the merge job list.
getMergeJobs :: BotState -> Set PatchId
getMergeJobs (BotState patches) = Map.keysSet $ Map.filter isMergeJob patches

-- | Get the options for the given patch.
getPatchOptions :: BotState -> PatchId -> Maybe PatchOptionsPartial
getPatchOptions (BotState patches) patch = case Map.lookup patch patches of
  Just TryJob -> Nothing
  Just (MergeQueue opts) -> Just opts
  Just (MergeJob opts) -> Just opts
  Nothing -> Nothing

{- Modifying state -}

-- | Add the given patch to the merge queue.
insertMergeQueue :: PatchId -> PatchOptionsPartial -> BotState -> BotState
insertMergeQueue patch opts (BotState patches) = BotState $ Map.insert patch patchState patches
  where
    patchState = MergeQueue opts

-- | Remove the given patch from the merge queue.
removeMergeQueue :: PatchId -> BotState -> BotState
removeMergeQueue patch (BotState patches) = BotState $ Map.update checkQueue patch patches
  where
    checkQueue (MergeQueue _) = Nothing
    checkQueue patchState = Just patchState

-- | Clear the merge jobs, either after a successful merge or after cancelling the merge job.
clearMergeJobs :: BotState -> BotState
clearMergeJobs (BotState patches) = BotState $ Map.filter (not . isMergeJob) patches

-- | Initialize a merge job with all the patches in the queue.
--
-- Assumes 'clearMergeJobs' has been run.
initMergeJob :: BotState -> BotState
initMergeJob (BotState patches) = BotState $ Map.map toMergeJob patches
  where
    toMergeJob (MergeQueue opts) = MergeJob opts
    toMergeJob (MergeJob _) = error "Found existing merge job in initMergeJob"
    toMergeJob patchState = patchState

-- | Start a try job for the given patch.
startTryJob :: PatchId -> BotState -> BotState
startTryJob patch (BotState patches) = BotState $ Map.insert patch TryJob patches
