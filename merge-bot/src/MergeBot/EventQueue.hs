{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MergeBot.EventQueue
  ( -- * Queueing events
    EventQueuesManager
  , initEventQueuesManager
  , queueEventWith
  , handleEventsWith
  ) where

import Control.Monad (forever, join)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, link)
import UnliftIO.STM (STM, atomically)

import MergeBot.EventQueue.Internal hiding (EventQueuesManager(..), WorkerQueue(..))
import MergeBot.EventQueue.Internal (EventQueuesManager, WorkerQueue)

-- | Add the given event to the global queue.
queueEventWith :: EventQueuesManager key event -> key -> event -> IO ()
queueEventWith eventQueuesManager eventKey event =
  -- TODO: (optimization) if the event is RefreshCheckRun, see if the same
  -- RefreshCheckRun event is already in the queue. If so, don't add the
  -- event.
  atomically $ queueGlobalEvent eventQueuesManager (eventKey, event)

-- | For each event coming in from the globalEventQueue, add it to the queue
-- corresponding to the event key.
--
-- If the event is the first one in the queue, spin off a worker thread to process
-- the event with the given handler. Otherwise, add it to the queue and have the
-- worker thread process it on the next loop. When no more events are in the queue,
-- stop the worker thread; when the next event for that event key gets queued, a
-- new worker thread will be spun up to handle it.
--
-- One should take special care to ensure that the given function does not throw
-- an error. If the worker thread throws an exception, the exception will be
-- rethrown.
--
-- A couple design decisions went into this:
--   * Each PR should have its own queue, as much as possible, to avoid merge bot
--     actions on a PR being delayed because of activity on other PRs
--   * Servant routes should not spin up worker threads
--   * A closed PR should not have a running worker thread. Instead of a worker
--     thread being started/stopped when a PR is created/opened, we'll spin up
--     worker threads on demand
handleEventsWith :: forall m key event. (MonadUnliftIO m, Ord key, Show key)
  => EventQueuesManager key event
  -> (key -> event -> m ())
  -> m ()
handleEventsWith eventQueuesManager f = forever $ atomicallyThenRun $ do
    (eventKey, event) <- getNextGlobalEvent eventQueuesManager
    mkPostQueueAction eventKey <$> addEventToWorkerQueue eventQueuesManager eventKey event
  where
    mkPostQueueAction :: key -> AddEventResult event -> m ()
    mkPostQueueAction eventKey = \case
      AddedToNewQueue workerQueue -> do
        workerThread <- async $ runWorker eventQueuesManager workerQueue eventKey f
        -- the worker shouldn't throw an exception, but if it happens to, crash this process
        link workerThread
        atomically $ registerWorkerThread eventQueuesManager eventKey workerThread

      AddedToExistingQueue _ -> return ()

-- | Read and process events from the given queue. When the queue is empty, clean up
-- eventWorkerQueues and eventWorkerThreads and exit.
runWorker :: (MonadUnliftIO m, Ord key)
  => EventQueuesManager key event
  -> WorkerQueue event
  -> key
  -> (key -> event -> m ())
  -> m ()
runWorker eventQueuesManager workerQueue eventKey f = go
  where
    go = atomicallyThenRun $ getNextWorkerEvent workerQueue >>= \case
      -- process event, then loop
      Just event -> return $ f eventKey event >> go

      -- finished everything in queue; clean up and exit
      Nothing -> do
        cleanupWorker eventQueuesManager eventKey
        return $ return ()

{- Utilities -}

-- Run STM actions atomically to build up an IO action, then run the IO action
atomicallyThenRun :: MonadUnliftIO m => STM (m a) -> m a
atomicallyThenRun = join . atomically
