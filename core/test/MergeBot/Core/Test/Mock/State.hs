{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Test.Mock.State
  ( MockState(..)
  , GHCommit(..)
  , GHTreeEntry(..)
  ) where

import Data.Aeson.QQ (aesonQQ)
import Data.GraphQL.Aeson (Value(..), fromObject')
import Data.GraphQL.TestUtils (MocksApi(..), mock)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import MergeBot.Core.GitHub (PaginatedResult(..))
import MergeBot.Core.GraphQL.API (API)
import qualified MergeBot.Core.GraphQL.Branch as Branch
import qualified MergeBot.Core.GraphQL.Branches as Branches
import MergeBot.Core.GraphQL.Enums.StatusState (StatusState(..))
import MergeBot.Core.Test.Utils (paginated)

{- GitHub state -}

-- | Data type storing a mocked GitHub state.
--
-- Invariants:
--  * all (`elem` map commitHash ghCommits) ghBranches
--  * all (`Map.member` ghTrees) $ map commitTree ghCommits
data MockState = MockState
  { ghCommits  :: [GHCommit]
  , ghBranches :: Map Text SHA -- ^ mapping of branch name to commit hash
  , ghTrees    :: Map SHA [GHTreeEntry]
  } deriving (Show)

instance MocksApi API MockState where
  mockWith state =
    [ (mock Branch.query, getBranch state . fromObject' "name")
    , (mock Branches.query, getBranches state . fromObject' "after")
    ]

{- GitHub types -}

type SHA = Text

data GHCommit = GHCommit
  { commitHash     :: SHA
  , commitMessage  :: Text
  , commitTree     :: SHA
  , commitContexts :: [(Text, StatusState)]
  } deriving (Show)

data GHTreeEntry = GHTreeEntry
  { entryPath     :: Text
  , entryContents :: Text
  } deriving (Show)

{- JSON encoding -}

-- | Get the result of a 'Branch' query.
getBranch :: MockState -> Text -> Value
getBranch state name =
  [aesonQQ|
    {
      "repository": {
        "ref": {
          "target": #{encodeBranch state name}
        }
      }
    }
  |]

-- | Get the result of a 'Branches' query.
getBranches :: MockState -> Maybe String -> Value
getBranches state after =
  [aesonQQ|
    {
      "repository": {
        "refs": {
          "pageInfo": {
            "hasNextPage": #{hasNext},
            "endCursor": #{nextCursor}
          },
          "nodes": #{map mkNode chunk}
        }
      }
    }
  |]
  where
    mkNode branchName =
      [aesonQQ|
        {
          "name": #{branchName},
          "target": #{encodeBranch state branchName}
        }
      |]
    PaginatedResult{..} = paginated (Map.keys $ ghBranches state) after

encodeBranch :: MockState -> Text -> Value
encodeBranch MockState{..} name = encodeCommit commit treeEntries
  where
    commitSHA = fromMaybe
      (error $ "No branch named: " ++ Text.unpack name)
      $ Map.lookup name ghBranches
    commit = case filter ((== commitSHA) . commitHash) ghCommits of
      [] -> error $ "No commit with hash: " ++ Text.unpack commitSHA
      [c] -> c
      _ -> error $ "Multiple commits with hash: " ++ Text.unpack commitSHA
    treeSHA = commitTree commit
    treeEntries = fromMaybe
      (error $ "No tree with hash: " ++ Text.unpack treeSHA)
      $ Map.lookup (commitTree commit) ghTrees

encodeCommit :: GHCommit -> [GHTreeEntry] -> Value
encodeCommit GHCommit{..} entries =
  [aesonQQ|
    {
      "oid": #{commitHash},
      "message": #{commitMessage},
      "tree": {
        "oid": #{commitTree},
        "entries": #{map fromEntry entries}
      },
      "status": {
        "contexts": #{map fromContext commitContexts}
      }
    }
  |]
  where
    fromContext (name, state) =
      [aesonQQ|
        {
          "context": #{name},
          "state": #{show state}
        }
      |]
    fromEntry GHTreeEntry{..} =
      [aesonQQ|
        {
          "name": #{entryPath},
          "object": {
            "text": #{entryContents}
          }
        }
      |]
