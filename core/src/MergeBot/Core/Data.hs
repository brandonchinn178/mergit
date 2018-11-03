{-|
Module      :  MergeBot.Core.Data
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the data types for data types sent back from the merge bot.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MergeBot.Core.Data where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type PullRequestId = Int

type HTML = Text

-- | A pull request as displayed in the pull request list.
data PullRequest = PullRequest
  { number  :: !PullRequestId
  , title   :: !Text
  , author  :: !Text
  , created :: !UTCTime
  , updated :: !UTCTime
  , status  :: !BotStatus
  } deriving (Show,Generic,ToJSON)

-- | A detailed pull request.
data PullRequestDetail = PullRequestDetail
  { number     :: !PullRequestId
  , title      :: !Text
  , author     :: !Text
  , created    :: !UTCTime
  , updated    :: !UTCTime
  , url        :: !Text
  , body       :: !HTML
  , commit     :: !Text
  , base       :: !Text
  , approved   :: !Bool
  , tryRun     :: !(Maybe TryRun)
  , mergeQueue :: !(Maybe [PullRequestSimple]) -- ^ includes self
  , mergeRun   :: !(Maybe MergeRun) -- ^ includes self
  , canTry     :: !Bool
  , canQueue   :: !Bool
  , canUnqueue :: !Bool
  } deriving (Show,Generic,ToJSON)

-- | A pull request with just the number and title.
data PullRequestSimple = PullRequestSimple
  { number :: !PullRequestId
  , title  :: !Text
  } deriving (Show,Generic,ToJSON)

-- | The status or result of a try run.
data TryRun = TryRun
  { tryStatus :: !CIStatus
  } deriving (Show,Generic,ToJSON)

-- | The status or result of a merge run.
data MergeRun = MergeRun
  { mergePRs    :: ![PullRequestSimple]
  , mergeStatus :: !CIStatus
  } deriving (Show,Generic,ToJSON)

-- | The status of a pull request according to the merge bot.
data BotStatus
  = Merging MergeStatus
  | MergeQueue
  | Trying TryStatus
  | None
  deriving (Show,Generic,ToJSON)

-- | The status of a try run.
data TryStatus = TrySuccess | TryRunning | TryFailed
  deriving (Show,Generic,ToJSON)

-- | The status of a merge run.
data MergeStatus = MergeRunning | MergeFailed
  deriving (Show,Generic,ToJSON)

-- | The status of a CI job.
data JobStatus = CISuccess | CIRunning | CIWaiting | CIFailed
  deriving (Show,Generic,ToJSON,Eq)

-- | The status of CI for a pull request.
newtype CIStatus = CIStatus [(Text, JobStatus)]
  deriving (Show,Generic,ToJSON)
