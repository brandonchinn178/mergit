import Test.Tasty

import qualified MergeBot.EventQueueTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "merge-bot"
      [ MergeBot.EventQueueTest.test
      ]
