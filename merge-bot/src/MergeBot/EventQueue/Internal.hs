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

import MergeBot.Core.GitHub (BranchName, CheckRunId, CommitSHA, PrNum, Repo)

-- | A merge bot event to be resolved serially.
--
-- In order to ensure that mergebot events are resolved atomically, merge-bot code shouldn't run
-- state-modifying events directly, but rather queue events to be run serially in a separate
-- thread.
data MergeBotEvent
  = PRCreated PrNum CommitSHA
  | CommitPushedToPR PrNum CommitSHA
  | StartTryJob PrNum CommitSHA BranchName CheckRunId
  | QueuePR PrNum CommitSHA
  | DequeuePR PrNum CommitSHA
  | ResetMerge PrNum CommitSHA
  | RefreshCheckRun BranchName CommitSHA
  | DeleteBranch BranchName
  | PollQueues
  deriving (Show, Eq)

{- EventKey -}

data EventKey
  = OnPR Repo PrNum
  | OnBranch Repo BranchName
  | OnRepo Repo
  deriving (Show, Eq, Ord)

makeEventKey :: Repo -> MergeBotEvent -> EventKey
makeEventKey repo = \case
  PRCreated prNum _        -> OnPR repo prNum
  CommitPushedToPR prNum _ -> OnPR repo prNum
  StartTryJob prNum _ _ _  -> OnPR repo prNum
  QueuePR prNum _          -> OnPR repo prNum
  DequeuePR prNum _        -> OnPR repo prNum
  ResetMerge prNum _       -> OnPR repo prNum
  RefreshCheckRun branch _ -> OnBranch repo branch
  DeleteBranch branch      -> OnBranch repo branch
  PollQueues               -> OnRepo repo

getEventRepo :: EventKey -> Repo
getEventRepo = \case
  OnPR repo _ -> repo
  OnBranch repo _ -> repo
  OnRepo repo -> repo

{- MergeBot queues -}

data MergeBotQueues = MergeBotQueues
  { globalEventQueue :: TBQueue (EventKey, MergeBotEvent)
  , workerQueues     :: TVar (Map EventKey WorkerQueue)
  }

initMergeBotQueues :: IO MergeBotQueues
initMergeBotQueues = atomically $ do
  globalEventQueue <- newTBQueue globalQueueLimit
  workerQueues <- newTVar Map.empty
  return MergeBotQueues{..}
  where
    globalQueueLimit = 1000

queueGlobalEvent :: MergeBotQueues -> (EventKey, MergeBotEvent) -> STM ()
queueGlobalEvent MergeBotQueues{..} = writeTBQueue globalEventQueue

getNextGlobalEvent :: MergeBotQueues -> STM (EventKey, MergeBotEvent)
getNextGlobalEvent MergeBotQueues{..} = readTBQueue globalEventQueue

{- Worker queues -}

type WorkerThread = Async ()

data WorkerQueue = WorkerQueue
  { workerEventQueue :: TBQueue MergeBotEvent
  , workerThread     :: Maybe WorkerThread
  }

data AddEventResult
  = AddedToExistingQueue WorkerQueue
  | AddedToNewQueue WorkerQueue

addEventToWorkerQueue :: MergeBotQueues -> EventKey -> MergeBotEvent -> STM AddEventResult
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

registerWorkerThread :: HasCallStack => MergeBotQueues -> EventKey -> WorkerThread -> STM ()
registerWorkerThread MergeBotQueues{..} eventKey workerThread =
  modifyTVar' workerQueues $ \queues ->
    case Map.lookup eventKey queues of
      Just workerQueue ->
        let workerQueue' = workerQueue { workerThread = Just workerThread }
        in Map.insert eventKey workerQueue' queues

      -- should not happen
      Nothing -> error $ "Event key not found in queues: " ++ show eventKey

getNextWorkerEvent :: WorkerQueue -> STM (Maybe MergeBotEvent)
getNextWorkerEvent WorkerQueue{..} = tryReadTBQueue workerEventQueue

cleanupWorker :: MergeBotQueues -> EventKey -> STM ()
cleanupWorker MergeBotQueues{..} eventKey = modifyTVar' workerQueues (Map.delete eventKey)
