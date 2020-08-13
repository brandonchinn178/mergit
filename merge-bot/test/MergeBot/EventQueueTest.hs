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
import Control.Concurrent.STM.TBQueue (flushTBQueue, isEmptyTBQueue)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar, swapTVar, writeTVar)
import Control.Monad (join, replicateM, unless, void)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Generic.Random (genericArbitrary, uniform)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GitHub.Data.GitObjectID (GitObjectID(..))
import Test.Tasty
import Test.Tasty.QuickCheck
import UnliftIO.Async
    (Async, async, asyncThreadId, race, uninterruptibleCancel, wait)
import UnliftIO.Exception (finally)

import MergeBot.EventQueue
import MergeBot.EventQueue.Internal

test :: TestTree
test = testGroup "MergeBot.EventQueue"
  [ testProperty "Can handle event after queueing it" $ \repo event ->
      ioProperty $ withEventQueues $ \getNextWorker -> do
        queueEvent repo event
        result <- getWorkerEvent <$> getNextWorker
        return $ result === (repo, event)

  , testProperty "Works across threads" $ \repo event ->
      ioProperty $ withEventQueues $ \getNextWorker -> do
        threadQueue <- makeThreadManager $ queueEvent repo event
        threadDequeue <- makeThreadManager $ getWorkerEvent <$> getNextWorker

        startThread threadQueue
        waitForThread threadQueue
        prop1 <- getThreadResult threadQueue ===^ Just ()
        prop2 <- getThreadResult threadDequeue ===^ Nothing

        startThread threadDequeue
        waitForThread threadDequeue
        prop3 <- getThreadResult threadQueue ===^ Just ()
        prop4 <- getThreadResult threadDequeue ===^ Just (repo, event)

        return $ conjoin [prop1, prop2, prop3, prop4]

  , testProperty "handleNextEvent blocks" $ \repo event ->
      ioProperty $ withEventQueues $ \getNextWorker -> do
        threadQueue <- makeThreadManager $ queueEvent repo event
        threadDequeue <- makeThreadManager $ getWorkerEvent <$> getNextWorker

        startThread threadDequeue
        prop1 <- getThreadResult threadQueue ===^ Nothing
        prop2 <- getThreadResult threadDequeue ===^ Nothing

        startThread threadQueue
        waitForThread threadQueue
        waitForThread threadDequeue
        prop3 <- getThreadResult threadQueue ===^ Just ()
        prop4 <- getThreadResult threadDequeue ===^ Just (repo, event)

        return $ conjoin [prop1, prop2, prop3, prop4]

  , testProperty "Events with the same EventKey run on the same thread" $ \eventKey ->
      forAll (listOf1 $ mergeBotEventWithKey eventKey) $ \events ->
        ioProperty $ withEventQueues $ \getNextWorker -> do
          let repo = getEventRepo eventKey
          mapM_ (queueEvent repo) events

          waitForGlobalQueueToBeProcessed

          workers <- replicateM (length events) getNextWorker

          return $ areAllSame $ map workerThreadId workers

  , testProperty "Events with different EventKeys run on different threads" $
      \repo1 event1 repo2 event2 -> ioProperty $ withEventQueues $ \getNextWorker -> do
        let eventKey1 = makeEventKey repo1 event1
            eventKey2 = makeEventKey repo2 event2

        queueEvent (getEventRepo eventKey1) event1
        queueEvent (getEventRepo eventKey2) event2

        worker1 <- getNextWorker
        worker2 <- getNextWorker

        return $ eventKey1 /= eventKey2 ==> workerThreadId worker1 /= workerThreadId worker2
  ]

{- MergeBot helpers -}

mergeBotEventWithKey :: EventKey -> Gen MergeBotEvent
mergeBotEventWithKey = \case
  OnPR _ prNum -> oneof
    [ applyArbitrary1 (PRCreated prNum)
    , applyArbitrary1 (CommitPushedToPR prNum)
    , applyArbitrary3 (StartTryJob prNum)
    , applyArbitrary1 (QueuePR prNum)
    , applyArbitrary1 (DequeuePR prNum)
    , applyArbitrary1 (ResetMerge prNum)
    ]
  OnBranch _ branch -> oneof
    [ applyArbitrary1 (RefreshCheckRun branch)
    , applyArbitrary0 (DeleteBranch branch)
    ]
  OnRepo _ -> oneof
    [ applyArbitrary0 PollQueues
    ]
  where
    applyArbitrary0 = pure
    applyArbitrary1 f = f <$> arbitrary

data EventWorker = EventWorker
  { workerThreadId :: ThreadId
  , workerEventKey :: EventKey
  , workerEvent    :: MergeBotEvent
  }

getWorkerEvent :: EventWorker -> (Repo, MergeBotEvent)
getWorkerEvent EventWorker{..} = (getEventRepo workerEventKey, workerEvent)

withEventQueues :: (IO EventWorker -> IO a) -> IO a
withEventQueues f = do
  clearAllQueuesAndThreads

  workers <- newTChanIO
  handleNextEvent <- newEmptyMVar

  eventThread <- async $ handleEvents $ \workerEventKey workerEvent -> do
    takeMVar handleNextEvent
    workerThreadId <- asyncThreadId <$> getWorkerThread workerEventKey
    atomically $ writeTChan workers EventWorker{..}

  let getNextWorker = withTimeout $ do
        putMVar handleNextEvent ()
        atomically $ readTChan workers

  f getNextWorker `finally` uninterruptibleCancel eventThread

waitForGlobalQueueToBeProcessed :: IO ()
waitForGlobalQueueToBeProcessed = atomically $ do
  isEmpty <- isEmptyTBQueue globalEventQueue
  unless isEmpty retry

getWorkerThread :: EventKey -> IO (Async ())
getWorkerThread eventKey = atomically $ do
  workerThreads <- readTVar eventWorkerThreads
  maybe retry return $ Map.lookup eventKey workerThreads

clearAllQueuesAndThreads :: IO ()
clearAllQueuesAndThreads = join $ atomically $ do
  void $ flushTBQueue globalEventQueue
  writeTVar eventWorkerQueues Map.empty
  oldThreads <- swapTVar eventWorkerThreads Map.empty
  return $ mapM_ uninterruptibleCancel $ Map.elems oldThreads

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
    -- timeout = threadDelay 5000000 -- 5 seconds
    timeout = threadDelay 10000000

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

{- Orphans -}

deriving instance Generic MergeBotEvent

instance Arbitrary MergeBotEvent where
  arbitrary = genericArbitrary uniform

deriving instance Generic EventKey

instance Arbitrary EventKey where
  arbitrary = genericArbitrary uniform

instance Arbitrary GitObjectID where
  arbitrary = GitObjectID <$> arbitrary

instance Arbitrary Text where
  arbitrary = Text.pack . getPrintableString <$> arbitrary
