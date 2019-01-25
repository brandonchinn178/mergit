{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Test.Mock
  ( MockData(..)
  , MockBranch(..)
  , MockPullRequest(..)
  , MockState
  , initialState
  , mockBranch
  , mockPR
  , toMockState
  , prettyState
  , createBranch
  , createCommit
  , deleteBranch
  , mergeBranches
  , updateBranch
  ) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as Text

import MergeBot.Core.Config (BranchConfig(..))
import MergeBot.Core.GraphQL.Enums.StatusState (StatusState(..))
import MergeBot.Core.Test.Mock.State

-- | Branches to seed initial GitHub state.
data MockBranch = MockBranch
  { branchName    :: BranchName
  , commitMessage :: Text
  , mergeConfig   :: Maybe BranchConfig
  , contexts      :: [(Text, StatusState)]
  } deriving (Show)

-- | A default 'MockBranch' to make it easy to only define specific fields.
--
-- Usage:
-- > mockBranch { branchName = "master" }
mockBranch :: MockBranch
mockBranch = MockBranch
  { branchName = "test"
  , commitMessage = "Some fix"
  , mergeConfig = Just $ BranchConfig []
  , contexts = []
  }

data MockPullRequest = MockPullRequest
  { number     :: Int
  , title      :: Text
  , author     :: Text
  , createdAt  :: Text
  , updatedAt  :: Text
  , body       :: Text
  , branch     :: BranchName
  , baseBranch :: BranchName
  } deriving (Show)

-- | A default 'MockPullRequest' to make it easy to only define specific fields.
--
-- Usage:
-- > mockPR { prNum = 1 }
mockPR :: MockPullRequest
mockPR = MockPullRequest
  { number = 1
  , title = "Test PR"
  , author = "alice"
  , createdAt = "2000-01-01T00:00:00Z"
  , updatedAt = "2000-01-01T00:00:00Z"
  , body = "This PR is very important"
  , branch = "test"
  , baseBranch = "master"
  }

-- | Interface for tests to seed initial GitHub state with mock data.
data MockData = MockData
  { mockBranches :: [MockBranch]
  , mockPRs      :: [MockPullRequest]
  } deriving (Show)

-- | Mock data to seed the initial state.
--
-- Usage:
-- > initialState { mockBranches = [mockBranch, mockBranch { branchName = "master" }] }
initialState :: MockData
initialState = MockData
  { mockBranches = []
  , mockPRs = []
  }

-- | Helper to convert the front-end interface for seeding state into the back-end representation of
-- GitHub state.
toMockState :: MockData -> MockState
toMockState MockData{..} = MockState{..}
  where
    numBranches = length mockBranches
    hashes offset = map (Text.pack . show) [offset * numBranches .. (offset + 1) * numBranches - 1]
    nextHash = 2 * numBranches
    ghCommits = Set.fromList $ map getCommit info
    ghBranches = Map.fromList $ map getBranch info
    ghTrees = Map.fromList $ map getTree info
    ghPRs = Set.fromList $ flip map mockPRs $ \MockPullRequest{..} -> GHPullRequest
      { prNum = number
      , prTitle = title
      , prAuthor = author
      , prCreated = createdAt
      , prUpdated = updatedAt
      , prBody = body
      , prCommitHash = fromMaybe
          (error $ "Non-existent branch for PR: " ++ show (number, branch))
          $ Map.lookup branch ghBranches
      , prBranch = branch
      , prBaseBranch = baseBranch
      }
    -- list of (branch, commitSHA, treeSHA, treeEntries)
    info = zipWith3 getInfo mockBranches (hashes 0) (hashes 1)
    getInfo branch commitSHA treeSHA =
      let toYAML = TextL.toStrict . Text.decodeUtf8 . encode
          entries = maybe Map.empty (Map.singleton ".lymerge.yaml" . toYAML) $ mergeConfig branch
      in (branch, commitSHA, treeSHA, entries)
    getCommit (MockBranch{..}, commitHash, commitTree, _) = GHCommit
      { commitParents = Nothing
      , commitContexts = contexts
      , ..
      }
    getBranch (branch, commitSHA, _, _) = (branchName branch, commitSHA)
    getTree (_, _, treeSHA, entries) = (treeSHA, entries)
