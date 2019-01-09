import Test.Tasty

import MergeBot.Core (getSessionInfo)
import MergeBot.Core.Monad (runBot)
import MergeBot.Core.Test

main :: IO ()
main = defaultMain $ testGroup "merge-bot-core-test"
  [ goldens "get_session_info" $ runBot testConfig getSessionInfo
  ]
