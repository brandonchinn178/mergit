import Test.Tasty

import qualified MergeBot.Core.GitHubTest

main :: IO ()
main = defaultMain $ testGroup "merge-bot-core"
  [ MergeBot.Core.GitHubTest.test
  ]
