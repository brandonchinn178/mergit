{-# LANGUAGE LambdaCase #-}

module MergeBot.EventQueue.Internal where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import GitHub.Data.GitObjectID (GitObjectID)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async (Async)
import UnliftIO.STM (STM)

import MergeBot.Core.GitHub (CheckRunId)

type Repo = (Text, Text)
type PrNum = Int
type BranchName = Text

-- | A merge bot event to be resolved serially.
--
-- In order to ensure that mergebot events are resolved atomically, merge-bot code shouldn't run
-- state-modifying events directly, but rather queue events to be run serially in a separate
-- thread.
data MergeBotEvent
  = PRCreated PrNum GitObjectID
  | CommitPushedToPR PrNum GitObjectID
  | StartTryJob PrNum GitObjectID BranchName CheckRunId
  | QueuePR PrNum GitObjectID
  | DequeuePR PrNum GitObjectID
  | ResetMerge PrNum GitObjectID
  | RefreshCheckRun BranchName GitObjectID
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

{- Global state -}

-- | The global queue of all events that occur in the merge bot.
globalEventQueue :: TBQueue (EventKey, MergeBotEvent)
globalEventQueue = unsafePerformIO $ newTBQueueIO globalQueueLimit
  where
    globalQueueLimit = 1000
{-# NOINLINE globalEventQueue #-}

-- | A registry of event queues per repo per possible PR.
eventWorkerQueues :: TVar (Map EventKey (TBQueue MergeBotEvent))
eventWorkerQueues = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE eventWorkerQueues #-}

-- | Look up the given EventKey in eventWorkerQueues.
getEventQueue :: EventKey -> STM (Maybe (TBQueue MergeBotEvent))
getEventQueue eventKey = Map.lookup eventKey <$> readTVar eventWorkerQueues

-- | The registry of worker threads.
--
-- Invariant: if an EventKey exists here, it's guaranteed to exist in eventWorkerQueues.
eventWorkerThreads :: TVar (Map EventKey (Async ()))
eventWorkerThreads = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE eventWorkerThreads #-}
