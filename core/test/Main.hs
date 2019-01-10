import Test.Tasty

import MergeBot.Core (getSessionInfo)
import qualified MergeBot.Core.Branch as Branch
import MergeBot.Core.Monad (runBot)
import MergeBot.Core.Test

main :: IO ()
main = defaultMain $ testGroup "merge-bot-core-test"
  [ goldens "get_session_info" $ runBot testConfig getSessionInfo
  , getBranchStatusesTests
  ]

getBranchStatusesTests :: TestTree
getBranchStatusesTests = testGroup "MergeBot.Core.Branch.getBranchStatuses"
  [ goldens "branch_statuses_try" $ runTestApp (Branch.getBranchStatuses []) mockData
      { mockBranches = [tryBranch 1]
      }
  ]
  where
    tryBranch :: Int -> Branch
    tryBranch pr = baseBranch { branchName = "trying-" ++ show pr }
