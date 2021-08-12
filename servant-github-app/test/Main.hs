import Test.Tasty

import qualified Servant.GitHub.CombinatorsTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "servant-github-app"
      [ Servant.GitHub.CombinatorsTest.test
      ]
