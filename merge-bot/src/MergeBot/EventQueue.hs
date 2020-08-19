{-# LANGUAGE LambdaCase #-}

module MergeBot.EventQueue
  ( MergeBotEvent(..)
  , queueEvent
  , handleEvents
  , EventKey
  , getEventRepo
  ) where

import Control.Concurrent.STM.TBQueue
    (TBQueue, newTBQueue, readTBQueue, tryReadTBQueue, writeTBQueue)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (forever, join)
import qualified Data.Map as Map
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, link)
import UnliftIO.STM (STM, atomically)

import MergeBot.EventQueue.Internal

-- | Add the given event to the global queue.
queueEvent :: Repo -> MergeBotEvent -> IO ()
queueEvent repo event =
  -- TODO: (optimization) if the event is RefreshCheckRun, see if the same
  -- RefreshCheckRun event is already in the queue. If so, don't add the
  -- event.
  atomically $ writeTBQueue globalEventQueue (makeEventKey repo event, event)

-- | For each event coming in from the globalEventQueue, add it to the queue
-- corresponding to the EventKey.
--
-- If the event is the first one in the queue, spin off a worker thread to process
-- the event with the given handler. Otherwise, add it to the queue and have the
-- worker thread process it on the next loop. When no more events are in the queue,
-- stop the worker thread; when the next event for that EventKey gets queued, a
-- new worker thread will be spun up to handle it.
--
-- One should take special care to ensure that the given function does not throw
-- an error. If the worker thread throws an exception, the queue will have undefined
-- behavior.
--
-- A couple design decisions went into this:
--   * Each PR should have its own queue, as much as possible, to avoid merge bot
--     actions on a PR being delayed because of activity on other PRs
--   * Servant routes should not spin up worker threads
--   * A closed PR should not have a running worker thread. Instead of a worker
--     thread being started/stopped when a PR is created/opened, we'll spin up
--     worker threads on demand
handleEvents :: MonadUnliftIO m => (EventKey -> MergeBotEvent -> m ()) -> m ()
handleEvents f = forever $ atomicallyThenRun $ do
  -- get the next event from the global queue
  (eventKey, event) <- readTBQueue globalEventQueue

  getEventQueue eventKey >>= \case
    Nothing -> do
      -- create a new worker queue and add the event to the queue
      queue <- newTBQueue workerQueueLimit
      writeTBQueue queue event

      -- Register worker queue with eventWorkerQueues
      modifyTVar' eventWorkerQueues (Map.insert eventKey queue)

      -- Start worker thread and register it with eventWorkerThreads
      return $ do
        worker <- async $ runWorker f eventKey queue
        link worker
        atomically $ modifyTVar' eventWorkerThreads (Map.insert eventKey worker)

    Just queue -> do
      -- add the event to the queue
      writeTBQueue queue event

      return $ return ()
  where
    workerQueueLimit = 1000

-- | Read and process events from the given queue. When the queue is empty, clean up
-- eventWorkerQueues and eventWorkerThreads and exit.
runWorker :: MonadUnliftIO m => (EventKey -> MergeBotEvent -> m ()) -> EventKey -> TBQueue MergeBotEvent -> m ()
runWorker f eventKey queue = go
  where
    go = atomicallyThenRun $ tryReadTBQueue queue >>= \case
      -- process event, then loop
      Just event -> return $ f eventKey event >> go

      -- finished everything in queue; clean up and exit
      Nothing -> do
        modifyTVar' eventWorkerQueues (Map.delete eventKey)
        modifyTVar' eventWorkerThreads (Map.delete eventKey)
        return $ return ()

{- Utilities -}

-- Run STM actions atomically to build up an IO action, then run the IO action
atomicallyThenRun :: MonadUnliftIO m => STM (m a) -> m a
atomicallyThenRun = join . atomically
