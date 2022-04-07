{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Mergit.EventQueue.Internal where

import Control.Concurrent.STM.TBQueue (
  TBQueue,
  newTBQueue,
  readTBQueue,
  tryReadTBQueue,
  writeTBQueue,
 )
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import UnliftIO.Async (Async)
import UnliftIO.STM (STM, atomically)

{- All queues -}

data EventQueuesManager key event = EventQueuesManager
  { globalEventQueue :: TBQueue (key, event)
  , workerQueues :: TVar (Map key (WorkerQueue event))
  , workerQueueLimit :: Natural
  }

data EventQueuesConfig = EventQueuesConfig
  { globalQueueLimit :: Natural
  , workerQueueLimit :: Natural
  }

initEventQueuesManager :: EventQueuesConfig -> IO (EventQueuesManager key event)
initEventQueuesManager EventQueuesConfig{..} = atomically $ do
  globalEventQueue <- newTBQueue globalQueueLimit
  workerQueues <- newTVar Map.empty
  return EventQueuesManager{..}

queueGlobalEvent :: EventQueuesManager key event -> (key, event) -> STM ()
queueGlobalEvent EventQueuesManager{..} = writeTBQueue globalEventQueue

getNextGlobalEvent :: EventQueuesManager key event -> STM (key, event)
getNextGlobalEvent EventQueuesManager{..} = readTBQueue globalEventQueue

{- Worker queues -}

type WorkerThread = Async ()

data WorkerQueue event = WorkerQueue
  { workerEventQueue :: TBQueue event
  , workerThread :: Maybe WorkerThread
  }

data AddEventResult event
  = AddedToExistingQueue (WorkerQueue event)
  | AddedToNewQueue (WorkerQueue event)

addEventToWorkerQueue :: Ord key => EventQueuesManager key event -> key -> event -> STM (AddEventResult event)
addEventToWorkerQueue EventQueuesManager{..} eventKey event = do
  queues <- readTVar workerQueues
  case Map.lookup eventKey queues of
    Nothing -> do
      -- create a new worker queue and add the event to the queue
      workerEventQueue <- newTBQueue workerQueueLimit
      writeTBQueue workerEventQueue event

      let workerQueue = WorkerQueue{workerEventQueue, workerThread = Nothing}

      -- register queue with workerQueues
      modifyTVar' workerQueues (Map.insert eventKey workerQueue)

      return $ AddedToNewQueue workerQueue
    Just workerQueue -> do
      -- add the event to the queue
      writeTBQueue (workerEventQueue workerQueue) event

      return $ AddedToExistingQueue workerQueue

registerWorkerThread :: (HasCallStack, Ord key, Show key) => EventQueuesManager key event -> key -> WorkerThread -> STM ()
registerWorkerThread EventQueuesManager{..} eventKey workerThread =
  modifyTVar' workerQueues $ \queues ->
    case Map.lookup eventKey queues of
      Just workerQueue ->
        let workerQueue' = workerQueue{workerThread = Just workerThread}
         in Map.insert eventKey workerQueue' queues
      -- should not happen
      Nothing -> error $ "Event key not found in queues: " ++ show eventKey

getNextWorkerEvent :: WorkerQueue event -> STM (Maybe event)
getNextWorkerEvent WorkerQueue{..} = tryReadTBQueue workerEventQueue

cleanupWorker :: Ord key => EventQueuesManager key event -> key -> STM ()
cleanupWorker EventQueuesManager{..} eventKey = modifyTVar' workerQueues (Map.delete eventKey)
