import Test.Tasty

import qualified Mergit.Core.ErrorTest
import qualified Mergit.Core.GitHubTest
import qualified Mergit.Core.TextTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "mergit-core"
      [ Mergit.Core.ErrorTest.test
      , Mergit.Core.GitHubTest.test
      , Mergit.Core.TextTest.test
      ]
