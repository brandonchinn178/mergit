import Test.Tasty

import Mergit.Core.ErrorTest qualified
import Mergit.Core.GitHubTest qualified
import Mergit.Core.TextTest qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "mergit-core"
      [ Mergit.Core.ErrorTest.test
      , Mergit.Core.GitHubTest.test
      , Mergit.Core.TextTest.test
      ]
