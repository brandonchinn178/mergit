import Test.Tasty

import qualified Mergit.EventQueueTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "mergit"
      [ Mergit.EventQueueTest.test
      ]
