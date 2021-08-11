import Test.Tasty

import qualified MergeBot.Core.GitHubTest
import qualified MergeBot.Core.TextTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "merge-bot-core"
      [ MergeBot.Core.GitHubTest.test
      , MergeBot.Core.TextTest.test
      ]
