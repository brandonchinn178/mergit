{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot
  ( PR(..)
  , PROption(..)
  , BotState(..)
  , newBotState
  , BotAction(..)
  , handleAction
  ) where

import Control.Monad (when)
import Data.List (partition)
import Data.Set (Set)

import MergeBot.Merge (MergeAlgorithm)

-- | The ID of a GitHub PR.
type PRId = Int

-- | The internal representation of a GitHub PR.
data PR = PR
  { prId   :: PRId
  , prOpts :: Set PROption
  } deriving (Show,Eq)

-- | Options specified per-PR to customize merge bot behavior.
data PROption
  = MergeAlgorithm MergeAlgorithm
  deriving (Show,Eq)

-- | The state of the merge bot.
data BotState = BotState
  { mergeQueue :: [PR]
  , mergeJobs  :: [PR]
  , tryJobs    :: [PRId]
  } deriving (Show,Eq)

newBotState :: BotState
newBotState = BotState
  { mergeQueue = []
  , mergeJobs = []
  , tryJobs = []
  }

-- | Errors that may occur in modifying the state of the merge bot.
data BotError
  = AlreadyInMergeQueue -- ^ Cannot add to merge queue twice
  | MergeJobStarted     -- ^ Cannot remove from merge queue if PR already running
  | TryJobStarted       -- ^ Cannot start a try job if PR already running a try job
  | DoesNotExist PRId   -- ^ The given PR does not exist
  deriving (Show,Eq)

-- | An action the user may send to the merge bot to modify the state.
data BotAction
  = AddMergeQueue PR
  | RemoveMergeQueue PRId
  | StartTryJob PRId
  deriving (Show,Eq)

handleAction :: BotAction -> BotState -> Either BotError BotState
handleAction action state@BotState{..} = case action of
  AddMergeQueue pr -> do
    when (pr `elem` mergeQueue) $ Left AlreadyInMergeQueue
    return $ state { mergeQueue = pr : mergeQueue }
  RemoveMergeQueue prId' -> do
    when (prId' `elem` map prId mergeJobs) $ Left MergeJobStarted
    case partition ((== prId') . prId) mergeQueue of
      ([], _) -> Left $ DoesNotExist prId'
      (_, mergeQueue') -> return $ state { mergeQueue = mergeQueue' }
  StartTryJob prId' -> do
    when (prId' `elem` tryJobs) $ Left TryJobStarted
    return $ state { tryJobs = prId' : tryJobs }

startMergeJob :: BotState -> BotState
startMergeJob state@BotState{..} = state { mergeQueue = [], mergeJobs = mergeQueue }
