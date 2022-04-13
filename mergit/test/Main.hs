import Test.Tasty

import Mergit.EventQueueTest qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "mergit"
      [ Mergit.EventQueueTest.test
      ]
