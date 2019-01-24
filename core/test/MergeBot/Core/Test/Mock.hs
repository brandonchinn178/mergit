{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Test.Mock
  ( MockData(..)
  , MockBranch(..)
  , MockState
  , initialState
  , mockBranch
  , toMockState
  , createBranch
  , createCommit
  , deleteBranch
  , mergeBranches
  , updateBranch
  ) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as Map
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
  { branchName    :: Text
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

-- | Interface for tests to seed initial GitHub state with mock data.
data MockData = MockData
  { mockBranches :: [MockBranch]
  } deriving (Show)

-- | Mock data to seed the initial state.
--
-- Usage:
-- > initialState { mockBranches = [mockBranch, mockBranch { branchName = "master" }] }
initialState :: MockData
initialState = MockData
  { mockBranches = []
  }

-- | Helper to convert the front-end interface for seeding state into the back-end representation of
-- GitHub state.
toMockState :: MockData -> MockState
toMockState MockData{..} = MockState
  { ghCommits = Set.fromList $ map getCommit info
  , ghBranches = Map.fromList $ map getBranch info
  , ghTrees = Map.fromList $ map getTree info
  }
  where
    hashes name = map (Text.pack . (name ++) . show) ([1..] :: [Int])
    -- list of (branch, commitSHA, treeSHA, treeEntries)
    info = zipWith3 getInfo mockBranches (hashes "commit") (hashes "tree")
    getInfo branch commitSHA treeSHA =
      let toYAML = TextL.toStrict . Text.decodeUtf8 . encode
          entries = maybe Map.empty (Map.singleton ".lymerge.yaml" . toYAML) $ mergeConfig branch
      in (branch, commitSHA, treeSHA, entries)
    getCommit (MockBranch{..}, commitHash, commitTree, _) = GHCommit{ commitContexts = contexts, .. }
    getBranch (branch, commitSHA, _, _) = (branchName branch, commitSHA)
    getTree (_, _, treeSHA, entries) = (treeSHA, entries)
