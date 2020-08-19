{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MergeBot.EventQueue
  ( -- * Queueing events
    MergeBotQueues
  , initMergeBotQueues
  , queueEventWith
  , handleEventsWith
    -- * EventKey
  , EventKey(..)
  , getEventRepo
  ) where

import Control.Monad (forever, join)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, link)
import UnliftIO.STM (STM, atomically)

import MergeBot.EventQueue.Internal hiding (MergeBotQueues(..), WorkerQueue(..))
import MergeBot.EventQueue.Internal (MergeBotQueues, WorkerQueue)

-- | Add the given event to the global queue.
queueEventWith :: MergeBotQueues event -> EventKey -> event -> IO ()
queueEventWith mergeBotQueues eventKey event =
  -- TODO: (optimization) if the event is RefreshCheckRun, see if the same
  -- RefreshCheckRun event is already in the queue. If so, don't add the
  -- event.
  atomically $ queueGlobalEvent mergeBotQueues (eventKey, event)

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
handleEventsWith :: forall m event. MonadUnliftIO m
  => MergeBotQueues event
  -> (EventKey -> event -> m ())
  -> m ()
handleEventsWith mergeBotQueues f = forever $ atomicallyThenRun $ do
    (eventKey, event) <- getNextGlobalEvent mergeBotQueues
    mkPostQueueAction eventKey <$> addEventToWorkerQueue mergeBotQueues eventKey event
  where
    mkPostQueueAction :: EventKey -> AddEventResult event -> m ()
    mkPostQueueAction eventKey = \case
      AddedToNewQueue workerQueue -> do
        workerThread <- async $ runWorker mergeBotQueues workerQueue eventKey f
        -- the worker shouldn't throw an exception, but if it happens to, crash this process
        link workerThread
        atomically $ registerWorkerThread mergeBotQueues eventKey workerThread

      AddedToExistingQueue _ -> return ()

-- | Read and process events from the given queue. When the queue is empty, clean up
-- eventWorkerQueues and eventWorkerThreads and exit.
runWorker :: MonadUnliftIO m
  => MergeBotQueues event
  -> WorkerQueue event
  -> EventKey
  -> (EventKey -> event -> m ())
  -> m ()
runWorker mergeBotQueues workerQueue eventKey f = go
  where
    go = atomicallyThenRun $ getNextWorkerEvent workerQueue >>= \case
      -- process event, then loop
      Just event -> return $ f eventKey event >> go

      -- finished everything in queue; clean up and exit
      Nothing -> do
        cleanupWorker mergeBotQueues eventKey
        return $ return ()

{- Utilities -}

-- Run STM actions atomically to build up an IO action, then run the IO action
atomicallyThenRun :: MonadUnliftIO m => STM (m a) -> m a
atomicallyThenRun = join . atomically
