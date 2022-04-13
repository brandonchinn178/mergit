import Test.Tasty

import Servant.GitHub.CombinatorsTest qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "servant-github-app"
      [ Servant.GitHub.CombinatorsTest.test
      ]
