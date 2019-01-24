module MergeBot.Core.Test
  ( testConfig
  , goldens
  , runTestApp'
  , module X
  ) where

import Control.Exception (try)
import Control.Monad.State.Lazy (get)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Network.HTTP.Client (HttpException)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.Test.Mock as X
import MergeBot.Core.Test.Monad as X

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

-- | Run the given action and then return either a server error or the final state at the end.
runTestApp' :: TestApp a -> MockData -> IO (Either HttpException MockState)
runTestApp' app = try . runTestApp (app >> get)
