{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (logInfoN)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID(..))
import qualified Network.Wai.Handler.Warp as Warp
import Servant
    ( Application
    , Context(..)
    , Handler
    , HasServer
    , ServerT
    , hoistServerWithContext
    , serveWithContext
    )
import Servant.GitHub (loadGitHubAppParams)
import UnliftIO (MonadUnliftIO, async, handle, waitAny)

import MergeBot.Auth (AuthParams(..), loadAuthParams)
import qualified MergeBot.Core as Core
import qualified MergeBot.Core.GitHub as Core
import MergeBot.Core.Monad (getRepo)
import MergeBot.EventQueue (MergeBotEvent(..), getEventRepo, handleEvents)
import MergeBot.Monad
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)

-- | Load environment variables and spin up all the merge bot threads.
runMergeBot :: IO ()
runMergeBot = concurrentlyAllIO
  [ handleBotQueue
  , pollMergeQueues
  , runServer
  ]
  where
    concurrentlyAllIO :: [BaseApp ()] -> IO ()
    concurrentlyAllIO actions = do
      ghAppParams <- loadGitHubAppParams
      authParams <- loadAuthParams
      runBaseApp ghAppParams authParams $ concurrentlyAll actions

handleBotQueue :: BaseApp ()
handleBotQueue = handleEvents handleBotEvent
  where
    handleBotEvent eventKey event = handle logException $ do
      let repo = getEventRepo eventKey
          (repoOwner, repoName) = repo

      token <- getTokenForRepo repo
      runBotApp repoOwner repoName (runBotEvent event) token

    runBotEvent = \case
      PRCreated prNum sha -> do
        logWithRepo $ "PR created: " <> showT prNum
        Core.createCheckRuns sha
      CommitPushedToPR prNum sha -> do
        logWithRepo $ "Commit pushed to PR: " <> showT prNum
        Core.createCheckRuns sha
      StartTryJob prNum sha branch checkRunId -> do
        logWithRepo $ "Trying PR #" <> showT prNum <> " for commit: " <> unOID sha
        Core.startTryJob prNum sha branch checkRunId
      QueuePR prNum sha -> do
        logWithRepo $ "Queuing PR #" <> showT prNum
        Core.queuePR prNum sha
      DequeuePR prNum sha -> do
        logWithRepo $ "Dequeuing PR #" <> showT prNum
        Core.dequeuePR prNum sha
      ResetMerge prNum sha -> do
        logWithRepo $ "Resetting merge check run for PR #" <> showT prNum
        Core.resetMerge prNum sha
      RefreshCheckRun branch sha -> do
        logWithRepo $ "Handling status update for " <> branch <> " (" <> unOID sha <> ")"
        Core.handleStatusUpdate branch sha
      DeleteBranch branch -> do
        logWithRepo $ "Deleting branch: " <> branch
        Core.deleteBranch branch
      PollQueues -> do
        logWithRepo "Polling queues"
        Core.pollQueues

    logWithRepo msg = do
      (repoOwner, repoName) <- getRepo
      let repo = repoOwner <> "/" <> repoName
      logInfoN $ "[" <> repo <> "] " <> msg

    -- TODO: calling getRepoAndTokens for each event is very inefficient, maybe using some token pool?
    getTokenForRepo repo = do
      mToken <- lookup repo <$> getRepoAndTokens
      maybe (error $ "Could not find repository: " ++ show repo) return mToken

    showT = Text.pack . show

-- TODO: instead of polling, have the completed merge run start the next merge run
pollMergeQueues :: BaseApp ()
pollMergeQueues = forever $ do
  handle logException $ void $ runBotAppOnAllRepos $ queueEvent' PollQueues

  liftIO $ threadDelay $ pollDelayMinutes * 60e6
  where
    pollDelayMinutes = 10

runServer :: BaseApp ()
runServer = do
  ghAppParams <- getGitHubAppParams
  authParams <- getAuthParams

  let runBaseHandler' = runBaseHandler ghAppParams authParams
      context = cookieSettings authParams :. jwtSettings authParams :. ghAppParams :. EmptyContext

  liftIO $ Warp.run 3000 $ serveRoutes @MergeBotRoutes runBaseHandler' context handleMergeBotRoutes

{- Helpers -}

logException :: MonadIO m => SomeException -> m ()
logException = liftIO . putStrLn . displayException

-- | Run each of the given actions in a separate thread.
--
-- If any action throws an exception, rethrow the exception.
concurrentlyAll :: MonadUnliftIO m => [m ()] -> m ()
concurrentlyAll actions = do
  threads <- mapM async actions
  void $ waitAny threads

serveRoutes :: forall api context m
  . (HasServer api context)
  => (forall x. m x -> Handler x) -> Context context -> ServerT api m -> Application
serveRoutes f context routes =
  serveWithContext (Proxy @api) context $
    hoistServerWithContext (Proxy @api) (Proxy @context) f routes
