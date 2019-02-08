{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (run)
import Servant (runHandler)

import MergeBot.Core (hasQueue, runMerge, startMergeJob)
import MergeBot.Core.Branch (getStagingBranches, getStagingStatus)
import MergeBot.Core.CIStatus (isSuccess)
import MergeBot.Core.State (BotState, newBotState)
import MergeBot.Server (initApp)
import MergeBot.Server.Monad (initEnv, runMergeBotHandler, updateBotState_)

main :: IO ()
main = do
  stateMVar <- newMVar newBotState
  pollCI stateMVar
  run 3000 =<< initApp stateMVar

-- | Poll CI to check if any staging branches should be merged.
--
-- This function should go away when using GitHub webhooks to detect when to merge PRs.
pollCI :: MVar BotState -> IO ()
pollCI stateMVar = void $ forkIO $ do
  env <- initEnv stateMVar
  void $ runHandler $ runMergeBotHandler env $ forever $ action >> sleepTenMinutes
  where
    sleepTenMinutes = liftIO $ threadDelay $ 10 * 60e6
    action = do
      baseBranches <- getStagingBranches
      forM_ baseBranches $ \baseBranch -> do
        ciStatus <- fromJust <$> getStagingStatus baseBranch
        when (isSuccess ciStatus) $ do
          runMerge baseBranch
          updateBotState_ $ \state ->
            if hasQueue baseBranch state
              then startMergeJob baseBranch state
              else return state
