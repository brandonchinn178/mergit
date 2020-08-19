{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.EventQueue.Internal where

import Control.Concurrent.STM.TBQueue
    (TBQueue, newTBQueue, readTBQueue, tryReadTBQueue, writeTBQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Stack (HasCallStack)
import UnliftIO.Async (Async)
import UnliftIO.STM (STM, atomically)

import MergeBot.Core.GitHub (BranchName, PrNum, Repo)

{- EventKey -}

data EventKey
  = OnPR Repo PrNum
  | OnBranch Repo BranchName
  | OnRepo Repo
  deriving (Show, Eq, Ord)

getEventRepo :: EventKey -> Repo
getEventRepo = \case
  OnPR repo _ -> repo
  OnBranch repo _ -> repo
  OnRepo repo -> repo

{- MergeBot queues -}

data MergeBotQueues event = MergeBotQueues
  { globalEventQueue :: TBQueue (EventKey, event)
  , workerQueues     :: TVar (Map EventKey (WorkerQueue event))
  }

initMergeBotQueues :: IO (MergeBotQueues event)
initMergeBotQueues = atomically $ do
  globalEventQueue <- newTBQueue globalQueueLimit
  workerQueues <- newTVar Map.empty
  return MergeBotQueues{..}
  where
    globalQueueLimit = 1000

queueGlobalEvent :: MergeBotQueues event -> (EventKey, event) -> STM ()
queueGlobalEvent MergeBotQueues{..} = writeTBQueue globalEventQueue

getNextGlobalEvent :: MergeBotQueues event -> STM (EventKey, event)
getNextGlobalEvent MergeBotQueues{..} = readTBQueue globalEventQueue

{- Worker queues -}

type WorkerThread = Async ()

data WorkerQueue event = WorkerQueue
  { workerEventQueue :: TBQueue event
  , workerThread     :: Maybe WorkerThread
  }

data AddEventResult event
  = AddedToExistingQueue (WorkerQueue event)
  | AddedToNewQueue (WorkerQueue event)

addEventToWorkerQueue :: MergeBotQueues event -> EventKey -> event -> STM (AddEventResult event)
addEventToWorkerQueue MergeBotQueues{..} eventKey event = do
  queues <- readTVar workerQueues
  case Map.lookup eventKey queues of
    Nothing -> do
      -- create a new worker queue and add the event to the queue
      workerEventQueue <- newTBQueue workerQueueLimit
      writeTBQueue workerEventQueue event

      let workerQueue = WorkerQueue { workerEventQueue, workerThread = Nothing }

      -- register queue with workerQueues
      modifyTVar' workerQueues (Map.insert eventKey workerQueue)

      return $ AddedToNewQueue workerQueue

    Just workerQueue -> do
      -- add the event to the queue
      writeTBQueue (workerEventQueue workerQueue) event

      return $ AddedToExistingQueue workerQueue
  where
    workerQueueLimit = 1000

registerWorkerThread :: HasCallStack => MergeBotQueues event -> EventKey -> WorkerThread -> STM ()
registerWorkerThread MergeBotQueues{..} eventKey workerThread =
  modifyTVar' workerQueues $ \queues ->
    case Map.lookup eventKey queues of
      Just workerQueue ->
        let workerQueue' = workerQueue { workerThread = Just workerThread }
        in Map.insert eventKey workerQueue' queues

      -- should not happen
      Nothing -> error $ "Event key not found in queues: " ++ show eventKey

getNextWorkerEvent :: WorkerQueue event -> STM (Maybe event)
getNextWorkerEvent WorkerQueue{..} = tryReadTBQueue workerEventQueue

cleanupWorker :: MergeBotQueues event -> EventKey -> STM ()
cleanupWorker MergeBotQueues{..} eventKey = modifyTVar' workerQueues (Map.delete eventKey)
