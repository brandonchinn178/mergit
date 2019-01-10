{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Test.Branch where

import Data.Aeson (ToJSON(..), Value, encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text.Lazy.Encoding as Text

import MergeBot.Core.Config (BranchConfig(..))
import MergeBot.Core.GitHub (PaginatedResult(..))
import MergeBot.Core.GraphQL.Enums.StatusState (StatusState(..))
import MergeBot.Core.Test.Utils (paginated)

data Branch = Branch
  { branchName    :: String
  , commitHash    :: String
  , commitMessage :: String
  , mergeConfig   :: Maybe BranchConfig
  , contexts      :: [(String, StatusState)]
  } deriving (Show)

baseBranch :: Branch
baseBranch = Branch
  { branchName = "test"
  , commitHash = "deadbeef"
  , commitMessage = "Some fix"
  , mergeConfig = Just $ BranchConfig []
  , contexts = []
  }

instance ToJSON Branch where
  toJSON Branch{..} =
    [aesonQQ|
      {
        "oid": #{commitHash},
        "message": #{commitMessage},
        "tree": {
          "oid": "01234567890abcdef",
          "entries": #{lymergeFile}
        },
        "status": {
          "contexts": #{map fromContext contexts}
        }
      }
    |]
    where
      lymergeFile = maybe [] ((:[]) . fromConfig) mergeConfig
      fromConfig config =
        [aesonQQ|
          {
            "name": ".lymerge.yaml",
            "object": {
              "text": #{Text.decodeUtf8 $ encode config}
            }
          }
        |]
      fromContext (name, state) =
        [aesonQQ|
          {
            "context": #{name},
            "state": #{show state}
          }
        |]

encodeBranch :: Branch -> Value
encodeBranch branch =
  [aesonQQ|
    {
      "repository": {
        "ref": {
          "target": #{branch}
        }
      }
    }
  |]

encodeBranches :: [Branch] -> Maybe String -> Value
encodeBranches branches after =
  [aesonQQ|
    {
      "repository": {
        "refs": {
          "pageInfo": {
            "hasNextPage": #{hasNext},
            "endCursor": #{nextCursor}
          },
          "nodes": #{map node chunk}
        }
      }
    }
  |]
  where
    node branch =
      [aesonQQ|
        {
          "name": #{branchName branch},
          "target": #{branch}
        }
      |]
    PaginatedResult{..} = paginated branches after
