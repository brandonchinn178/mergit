{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import MergeBot.Core (getSessionInfo)
import qualified MergeBot.Core.Branch as Branch
import MergeBot.Core.Config (BranchConfig(..))
import qualified MergeBot.Core.GraphQL.Enums.StatusState as StatusState
import MergeBot.Core.Monad (runBot)
import MergeBot.Core.Test

main :: IO ()
main = defaultMain $ testGroup "merge-bot-core-test"
  [ goldens "get_session_info" $ runBot testConfig getSessionInfo
  , getBranchStatusesTests
  ]

getBranchStatusesTests :: TestTree
getBranchStatusesTests = testGroup "MergeBot.Core.Branch.getBranchStatuses"
  [ testBranchStatuses "branch_statuses_try" [] [tryBranch]
  , testBranchStatuses "branch_statuses_try_config" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          }
      ]
  , testBranchStatuses "branch_statuses_try_no_config" []
      [ tryBranch
          { contexts = [("test1", StatusState.SUCCESS)]
          }
      ]
  , testBranchStatuses "branch_statuses_try_config_other" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("other", StatusState.SUCCESS)]
          }
      ]
  , testBranchStatuses "branch_statuses_try_config_pending" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.PENDING)]
          }
      ]
  , testBranchStatuses "branch_statuses_try_config_failure" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.FAILURE)]
          }
      ]
  , testBranchStatuses "branch_statuses_try_config_success" []
      [ tryBranch
          { mergeConfig = Just $ BranchConfig ["test1"]
          , contexts = [("test1", StatusState.SUCCESS)]
          }
      ]
  ]
  where
    testBranchStatuses name queue branches = goldens name $
      runTestApp (Branch.getBranchStatuses queue) mockData { mockBranches = branches }
    tryName pr = "trying-" ++ show (pr :: Int)
    tryBranch = baseBranch { branchName = tryName 1 }
