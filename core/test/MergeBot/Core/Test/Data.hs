{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Test.Data where

import Data.GraphQL.Aeson (fromObject')
import Data.GraphQL.TestUtils (MocksApi(..), mock)
import qualified Data.Text as Text

import MergeBot.Core.GraphQL.API (API)
import qualified MergeBot.Core.GraphQL.Branch as Branch
import qualified MergeBot.Core.GraphQL.Branches as Branches
import MergeBot.Core.Test.Branch (Branch(..), encodeBranch, encodeBranches)

-- | Data to use to mock GitHub state in tests.
data MockData = MockData
  { mockBranches :: [Branch]
  } deriving (Show)

-- | Default mock data.
mockData :: MockData
mockData = MockData
  { mockBranches = []
  }

instance MocksApi API MockData where
  mockWith MockData{..} =
    [ (mock Branch.query, getBranch mockBranches . fromObject' "name")
    , (mock Branches.query, encodeBranches mockBranches . fromObject' "after")
    ]
    where
      getBranch branches name = case filter ((== name) . branchName) branches of
        [] -> error $ "No branch named: " ++ Text.unpack name
        branch:_ -> encodeBranch branch
