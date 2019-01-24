{-# LANGUAGE OverloadedStrings #-}

module Branch where

import Data.Text (Text)
import Test.Tasty

import qualified MergeBot.Core.Branch as Branch
import MergeBot.Core.Branch.Internal
    (toStagingBranch, toStagingMessage, toTryBranch)
import MergeBot.Core.Config (BranchConfig(..))
import qualified MergeBot.Core.GraphQL.Enums.StatusState as StatusState
import MergeBot.Core.Test

tests :: TestTree
tests = testGroup "Branch"
  [ getBranchStatusesTests
  , getTryStatusTests
  , getStagingStatusTests
  , createTryBranchTests
  ]

getBranchStatusesTests :: TestTree
getBranchStatusesTests = testGroup "getBranchStatuses"
  [ testBranchStatuses "try" [] [tryBranch]
  , testBranchStatuses "try_config" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          }
      ]
  , testBranchStatuses "try_no_config" []
      [ tryBranch
          { contexts = [("test1", StatusState.SUCCESS)]
          }
      ]
  , testBranchStatuses "try_config_other" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("other", StatusState.SUCCESS)]
          }
      ]
  , testBranchStatuses "try_config_pending" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.PENDING)]
          }
      ]
  , testBranchStatuses "try_config_failure" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.FAILURE)]
          }
      ]
  , testBranchStatuses "try_config_success" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.SUCCESS)]
          }
      ]
  , testBranchStatuses "queue" [1] [tryBranch]
  , testBranchStatuses "queue_try" [1]
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.PENDING)]
          }
      ]
  , testBranchStatuses "staging" []
      [ stagingBranch [2, 3]
      ]
  , testBranchStatuses "staging_failed" []
      [ (stagingBranch [2, 3])
          { contexts = [("test1", StatusState.FAILURE)]
          }
      ]
  , testBranchStatuses "staging_queue" [1]
      [ tryBranch
      , stagingBranch [2, 3]
      ]
  , testBranchStatuses "staging_queue_try" [4]
      [ tryBranch
      , stagingBranch [2, 3]
      , mockBranch { branchName = toTryBranch 4 }
      ]
  ]
  where
    testBranchStatuses name queue branches = goldens ("branch_statuses_" ++ name) $
      runTestApp (Branch.getBranchStatuses queue) initialState { mockBranches = branches }
    tryBranch = mockBranch { branchName = toTryBranch 1 }
    stagingBranch prs = mockBranch
      { branchName = toStagingBranch "master"
      , commitMessage = toStagingMessage prs
      , mergeConfig = Just $ BranchConfig ["test1"]
      , contexts = [("test1", StatusState.PENDING)]
      }

getTryStatusTests :: TestTree
getTryStatusTests =
  getStatusTests "getTryStatus" (Branch.getTryStatus 1) (toTryBranch 1)

getStagingStatusTests :: TestTree
getStagingStatusTests =
  getStatusTests "getStagingStatus" (Branch.getStagingStatus "master") (toStagingBranch "master")

getStatusTests :: Show a => String -> TestApp a -> Text -> TestTree
getStatusTests groupName action branchName' = testGroup groupName
  [ testTryStatus "empty" []
  , testTryStatus "test1_expected" [("test1", StatusState.EXPECTED)]
  , testTryStatus "test1_error" [("test1", StatusState.ERROR)]
  , testTryStatus "test1_failure" [("test1", StatusState.FAILURE)]
  , testTryStatus "test1_pending" [("test1", StatusState.PENDING)]
  , testTryStatus "test1_success" [("test1", StatusState.SUCCESS)]
  , testTryStatus "all_success" [("test1", StatusState.SUCCESS), ("test2", StatusState.SUCCESS)]
  , testTryStatus "test2_failure" [("test1", StatusState.SUCCESS), ("test2", StatusState.FAILURE)]
  ]
  where
    testTryStatus name contexts' = goldens ("get_status_" ++ name) $
      runTestApp action initialState
        { mockBranches =
          [ mockBranch
            { branchName = branchName'
            , mergeConfig = Just $ BranchConfig ["test1", "test2"]
            , contexts = contexts'
            }
          ]
        }

createTryBranchTests :: TestTree
createTryBranchTests = testGroup "createTryBranch"
  [ testCreateTry "with_base" initialState
    { mockBranches =
      [ mockBranch { branchName = "master", mergeConfig = Just $ BranchConfig ["test1"] }
      , mockBranch { branchName = "test" }
      ]
    , mockPRs =
      [ mockPR { number = 1 }
      ]
    }
  ]
  where
    testCreateTry name state = goldens ("create_try_" ++ name) $
      runTestApp' (Branch.createTryBranch "master" 1) state
