{-# LANGUAGE DataKinds #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (run)

import MergeBot.Core (runMerge)
import MergeBot.Core.Branch (getStagingBranches, getStagingStatus)
import MergeBot.Core.CIStatus (isSuccess)
import MergeBot.Core.Monad (runBot)
import MergeBot.Server (initApp)
import MergeBot.Server.Monad (MergeBotEnv(..), initEnv)

main :: IO ()
main = do
  pollCI
  run 3000 =<< initApp

-- | Poll CI to check if any staging branches should be merged.
--
-- This function should go away when using GitHub webhooks to detect when to merge PRs.
pollCI :: IO ()
pollCI = void $ forkIO $ do
  config <- botConfig <$> initEnv
  runBot config $ forever $ action >> sleepTenMinutes
  where
    sleepTenMinutes = liftIO $ threadDelay 600000000
    action = do
      baseBranches <- getStagingBranches
      forM_ baseBranches $ \baseBranch -> do
        ciStatus <- fromJust <$> getStagingStatus baseBranch
        when (isSuccess ciStatus) $ runMerge baseBranch
