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
  , createBranch
  , createCommit
  , deleteBranch
  , mergeBranches
  , updateBranch
  ) where

import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Lazy (MonadState, get, put)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.GraphQL.Aeson (Value(..), fromObject')
import Data.GraphQL.TestUtils (MocksApi(..), mock)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client.Internal
    ( HttpException(..)
    , HttpExceptionContent(..)
    , Response(..)
    , ResponseClose(..)
    )
import Network.HTTP.Types (http10, status422)

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
  { ghCommits  :: Set GHCommit
  , ghBranches :: Map Text SHA -- ^ mapping of branch name to commit hash
  , ghTrees    :: Map SHA GHTree
  } deriving (Show)

instance MocksApi API MockState where
  mockWith state =
    [ (mock Branch.query, getBranch state . fromObject' "name")
    , (mock Branches.query, getBranches state . fromObject' "after")
    ]

{- REST endpoints -}

createBranch :: (MonadThrow m, MonadState MockState m) => Text -> SHA -> m Value
createBranch branchRef commitSHA = do
  state@MockState{..} <- get
  branchName <- maybe
    (ghThrow state branchRef $ show branchRef ++ " is not a valid ref name.")
    return
    $ Text.stripPrefix "refs/heads/" branchRef

  when (branchName `Map.member` ghBranches) $
    ghThrow state branchName "Reference already exists."
  unless (any (isCommit commitSHA) ghCommits) $
    ghThrow state commitSHA "Object does not exist"

  put state{ ghBranches = Map.insert branchName commitSHA ghBranches }
  return Null

createCommit :: (MonadThrow m, MonadState MockState m) => Text -> SHA -> [Text] -> m Value
createCommit commitMessage commitTree parents = do
  state@MockState{..} <- get

  unless (commitTree `Map.member` ghTrees) $
    ghThrow state commitTree "Tree SHA does not exist"
  when (any (isCommit commitHash) ghCommits) $
    fail $ "Creating commit with existing SHA: " ++ show (commitHash, state)

  put state{ ghCommits = Set.insert GHCommit{..} ghCommits }
  return [aesonQQ| { "sha": #{commitHash} } |]
  where
    commitHash = Text.intercalate "-"
      ["new-commit", commitMessage, commitTree, Text.intercalate "+" parents]
    commitContexts = []

deleteBranch :: (MonadThrow m, MonadState MockState m) => Text -> m Value
deleteBranch branchRef = do
  state@MockState{..} <- get
  let name = fromMaybe branchRef $ Text.stripPrefix "heads/" branchRef

  unless (name `Map.member` ghBranches) $
    ghThrow state name "Reference does not exist"

  put state{ ghBranches = Map.delete name ghBranches }
  return Null

mergeBranches :: (MonadThrow m, MonadState MockState m) => Text -> Text -> Text -> m Value
mergeBranches base commitHead commitMessage = do
  state@MockState{..} <- get
  -- the merge endpoint can take `base` as either refs/heads/NAME or just NAME
  let base' = fromMaybe base $ Text.stripPrefix "refs/heads/" base
      getTree sha = case Set.toList $ Set.filter (isCommit sha) ghCommits of
        [] -> ghThrow state commitHead "Head does not exist"
        [commit] -> case commitTree commit `Map.lookup` ghTrees of
          Nothing -> fail $ "Tree SHA does not exist: " ++ show (commitTree commit, state)
          Just entries -> return entries
        _ -> fail $ "Multiple commits with same SHA: " ++ show (commitHead, state)

  tree1 <- case base' `Map.lookup` ghBranches of
    Nothing -> ghThrow state base' "Base does not exist"
    Just baseCommitSHA -> getTree baseCommitSHA

  tree2 <- getTree commitHead

  let commitHash = Text.intercalate "-" ["merge-commit", base, commitHead]
      commitTree = Text.intercalate "-" ["merge-tree", base, commitHead]
      commitContexts = []
      treeEntries = doMerge tree1 tree2

  when (any (isCommit commitHash) ghCommits) $
    fail $ "Creating commits with existing SHA: " ++ show (commitHash, state)
  when (commitTree `Map.member` ghTrees) $
    fail $ "Creating tree with existing SHA: " ++ show (commitHash, state)

  put state
    { ghCommits = Set.insert GHCommit{..} ghCommits
    , ghTrees = Map.insert commitTree treeEntries ghTrees
    }
  return Null
  where
    -- Merge algorithm will just be tree1 overwritten by tree2
    doMerge tree1 tree2 = Map.union tree2 tree1

updateBranch :: (MonadThrow m, MonadState MockState m) => Text -> SHA -> m Value
updateBranch branchRef commitSHA = do
  state@MockState{..} <- get
  let name = fromMaybe branchRef $ Text.stripPrefix "heads/" branchRef

  unless (name `Map.member` ghBranches) $
    ghThrow state name "Reference does not exist"
  unless (any (isCommit commitSHA) ghCommits) $
    ghThrow state commitSHA "Object does not exist"

  put state{ ghBranches = Map.adjust (const commitSHA) name ghBranches }
  return Null

{- REST helpers -}

isCommit :: SHA -> GHCommit -> Bool
isCommit sha = (== sha) . commitHash

-- | Throws an error from GitHub.
--
-- The error message should match the actual error message, with the second argument giving
-- additional information that might be useful when debugging tests.
--
-- https://developer.github.com/v3/#client-errors
ghThrow :: (MonadThrow m, Show a) => MockState -> a -> String -> m b
ghThrow state extra message = throwM $ HttpExceptionRequest req content
  where
    req = error "No request instantiated"
    resp = Response
      { responseStatus = status422
      , responseVersion = http10
      , responseHeaders = []
      , responseBody = ()
      , responseCookieJar = mempty
      , responseClose' = ResponseClose $ pure ()
      }
    content = StatusCodeException resp $ ByteStringL.toStrict $ encode $ object
      [ "message" .= message
      , "extra" .= show extra
      , "state" .= show state
      ]

{- GitHub types -}

type SHA = Text

data GHCommit = GHCommit
  { commitHash     :: SHA
  , commitMessage  :: Text
  , commitTree     :: SHA
  , commitContexts :: [(Text, StatusState)]
  } deriving (Show,Eq,Ord)

type GHTree = Map FilePath Text

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
    commit = case Set.toList $ Set.filter (isCommit commitSHA) ghCommits of
      [] -> error $ "No commit with hash: " ++ Text.unpack commitSHA
      [c] -> c
      _ -> error $ "Multiple commits with hash: " ++ Text.unpack commitSHA
    treeSHA = commitTree commit
    treeEntries = fromMaybe
      (error $ "No tree with hash: " ++ Text.unpack treeSHA)
      $ Map.lookup (commitTree commit) ghTrees

encodeCommit :: GHCommit -> GHTree -> Value
encodeCommit GHCommit{..} entries =
  [aesonQQ|
    {
      "oid": #{commitHash},
      "message": #{commitMessage},
      "tree": {
        "oid": #{commitTree},
        "entries": #{map fromEntry $ Map.toList entries}
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
    fromEntry (entryPath, entryContents) =
      [aesonQQ|
        {
          "name": #{entryPath},
          "object": {
            "text": #{entryContents}
          }
        }
      |]
