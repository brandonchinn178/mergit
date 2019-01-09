module MergeBot.Core.Test where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import MergeBot.Core.Config (BotConfig(..))

-- | A BotConfig for testing
testConfig :: BotConfig
testConfig = BotConfig
  { cfgRepoOwner = "LeapYear"
  , cfgRepoName = "merge-bot-test"
  , cfgToken = ""
  }

goldens :: Show a => String -> IO a -> TestTree
goldens name = goldenVsString name fp . fmap (ByteString.pack . show)
  where
    fp = "test/goldens/" ++ name ++ ".golden"
