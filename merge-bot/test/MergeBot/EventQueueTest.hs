{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MergeBot.EventQueueTest where

import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TBQueue (isEmptyTBQueue)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Monad (forM_, replicateM, unless)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Test.Tasty
import Test.Tasty.QuickCheck
import UnliftIO.Async (async, asyncThreadId, race, uninterruptibleCancel, wait)
import UnliftIO.Exception (finally)

import MergeBot.EventQueue
import MergeBot.EventQueue.Internal

test :: TestTree
test = testGroup "MergeBot.EventQueue"
  [ testProperty "Can handle event after queueing it" $ \eventKey event ->
      ioProperty $ withEventQueues $ \EventQueuesTester{..} -> do
        queueEvent eventKey event
        result <- getWorkerEvent <$> getNextWorker
        return $ result === (eventKey, event)

  , testProperty "Works across threads" $ \eventKey event ->
      ioProperty $ withEventQueues $ \EventQueuesTester{..} -> do
        threadQueue <- makeThreadManager $ queueEvent eventKey event
        threadDequeue <- makeThreadManager $ getWorkerEvent <$> getNextWorker

        startThread threadQueue
        waitForThread threadQueue
        prop1 <- getThreadResult threadQueue ===^ Just ()
        prop2 <- getThreadResult threadDequeue ===^ Nothing

        startThread threadDequeue
        waitForThread threadDequeue
        prop3 <- getThreadResult threadQueue ===^ Just ()
        prop4 <- getThreadResult threadDequeue ===^ Just (eventKey, event)

        return $ conjoin [prop1, prop2, prop3, prop4]

  , testProperty "handleNextEvent blocks" $ \eventKey event ->
      ioProperty $ withEventQueues $ \EventQueuesTester{..} -> do
        threadQueue <- makeThreadManager $ queueEvent eventKey event
        threadDequeue <- makeThreadManager $ getWorkerEvent <$> getNextWorker

        startThread threadDequeue
        prop1 <- getThreadResult threadQueue ===^ Nothing
        prop2 <- getThreadResult threadDequeue ===^ Nothing

        startThread threadQueue
        waitForThread threadQueue
        waitForThread threadDequeue
        prop3 <- getThreadResult threadQueue ===^ Just ()
        prop4 <- getThreadResult threadDequeue ===^ Just (eventKey, event)

        return $ conjoin [prop1, prop2, prop3, prop4]

  , testProperty "Events with the same event key run on the same thread" $ \eventKey events ->
      ioProperty $ withEventQueues $ \EventQueuesTester{..} -> do
        mapM_ (queueEvent eventKey) (events :: [TestEvent])

        waitForGlobalQueueToBeProcessed

        workers <- replicateM (length events) getNextWorker

        return $ areAllSame $ map workerThreadId workers

  , testProperty "Events with different event keys run on different threads" $ \eventAndKey1 eventAndKey2 ->
      ioProperty $ withEventQueues $ \EventQueuesTester{..} -> do
        let (eventKey1, event1) = eventAndKey1
            (eventKey2, event2) = eventAndKey2

        queueEvent eventKey1 event1
        queueEvent eventKey2 event2

        worker1 <- getNextWorker
        worker2 <- getNextWorker

        return $ eventKey1 /= eventKey2 ==> workerThreadId worker1 /= workerThreadId worker2
  ]

{- Queueing helpers -}

newtype TestEventKey = TestEventKey Text
  deriving (Show,Eq,Ord)

instance Arbitrary TestEventKey where
  arbitrary = TestEventKey . Text.pack . getPrintableString <$> arbitrary

data TestEvent = TestEvent
  { testEventId   :: Int
  , testEventName :: String
  }
  deriving (Show,Eq)

instance Arbitrary TestEvent where
  arbitrary = TestEvent <$> arbitrary <*> arbitrary

data EventWorker = EventWorker
  { workerThreadId :: ThreadId
  , workerEventKey :: TestEventKey
  , workerEvent    :: TestEvent
  }

getWorkerEvent :: EventWorker -> (TestEventKey, TestEvent)
getWorkerEvent EventWorker{..} = (workerEventKey, workerEvent)

data EventQueuesTester = EventQueuesTester
  { getNextWorker                   :: IO EventWorker
  , waitForGlobalQueueToBeProcessed :: IO ()
  , queueEvent                      :: TestEventKey -> TestEvent -> IO ()
  }

withEventQueues :: (EventQueuesTester -> IO a) -> IO a
withEventQueues f = do
  eventQueuesManager <- initEventQueuesManager EventQueuesConfig
    { globalQueueLimit = 1000
    , workerQueueLimit = 1000
    }

  workers <- newTChanIO
  handleNextEvent <- newEmptyMVar

  eventThread <- async $ handleEventsWith eventQueuesManager $ \workerEventKey workerEvent -> do
    takeMVar handleNextEvent

    workerThreadId <- atomically $ do
      workerThreads <- readTVar $ workerQueues eventQueuesManager
      case Map.lookup workerEventKey workerThreads of
        Just WorkerQueue{workerThread = Just worker} -> return $ asyncThreadId worker
        _ -> retry

    atomically $ writeTChan workers EventWorker{..}

  let eventQueuesTester = EventQueuesTester
        { getNextWorker = withTimeout $ do
            putMVar handleNextEvent ()
            atomically $ readTChan workers
        , waitForGlobalQueueToBeProcessed = atomically $ do
            isEmpty <- isEmptyTBQueue $ globalEventQueue eventQueuesManager
            unless isEmpty retry
        , queueEvent = queueEventWith eventQueuesManager
        }

  f eventQueuesTester `finally` (do
    uninterruptibleCancel eventThread
    queues <- readTVarIO $ workerQueues eventQueuesManager
    forM_ queues $ \WorkerQueue{..} -> traverse uninterruptibleCancel workerThread
    )

{- Threading helpers -}

data ThreadManager a = ThreadManager
  { startThread     :: IO ()
  , waitForThread   :: IO ()
  , getThreadResult :: IO (Maybe a)
  }

makeThreadManager :: IO a -> IO (ThreadManager a)
makeThreadManager action = do
  mvarStart <- newEmptyMVar
  mvarResult <- newEmptyMVar

  asyncRef <- async $ do
    takeMVar mvarStart
    action >>= putMVar mvarResult

  return ThreadManager
    { startThread = putMVar mvarStart ()
    , waitForThread = wait asyncRef
    , getThreadResult = tryReadMVar mvarResult
    }

withTimeout :: HasCallStack => IO a -> IO a
withTimeout m = race timeout m >>= \case
  Left _ -> error "Timed out"
  Right x -> return x
  where
    timeout = threadDelay 5000000 -- 5 seconds

{- QuickCheck utilities -}

(===^) :: (Monad m, Show a, Eq a) => m a -> a -> m Property
m ===^ expected = do
  actual <- m
  return $ actual === expected

areAllSame :: (Show a, Eq a) => [a] -> Property
areAllSame [] = property True
areAllSame xs = allEqual (head xs) xs

allEqual :: (Show a, Eq a) => a -> [a] -> Property
allEqual expected xs = counterexample msg $ all (== expected) xs
  where
    msg = "Expected all to be " ++ show expected ++ ", got: " ++ show xs
