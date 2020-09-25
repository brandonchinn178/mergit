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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, fromException)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (logInfoN)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID(..))
import qualified Network.Wai.Handler.Warp as Warp
import Servant
    ( Application
    , Context(..)
    , Handler
    , HasServer
    , ServerError(..)
    , ServerT
    , err500
    , hoistServerWithContext
    , serveWithContext
    , throwError
    )
import Servant.GitHub (loadGitHubAppParams)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async (async, waitAny)
import UnliftIO.Exception (handle, try)

import MergeBot.Auth (AuthParams(..), loadAuthParams)
import qualified MergeBot.Core as Core
import qualified MergeBot.Core.GitHub as Core
import MergeBot.Core.Monad (getRepo)
import MergeBot.EventQueue (getEventRepo, initMergeBotQueues)
import MergeBot.Monad
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)

-- | Load environment variables and spin up all the merge bot threads.
runMergeBot :: IO ()
runMergeBot = do
  ghAppParams <- loadGitHubAppParams
  authParams <- loadAuthParams
  mergeBotQueues <- initMergeBotQueues

  runBaseApp BaseAppConfig{..} $ concurrentlyAll
    [ handleBotQueue
    , pollMergeQueues
    , runServer
    ]

handleBotQueue :: BaseApp ()
handleBotQueue = handleEvents handleBotEvent
  where
    handleBotEvent eventKey event = handle logException $ do
      let repo = getEventRepo eventKey

      token <- getTokenForRepo repo
      runBotApp repo (runBotEvent event) token

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
  handle logException $ void $ runBotAppOnAllRepos $ queueEvent PollQueues

  liftIO $ threadDelay $ pollDelayMinutes * 60e6
  where
    pollDelayMinutes = 1

runServer :: BaseApp ()
runServer = do
  ghAppParams <- getGitHubAppParams
  authParams <- getAuthParams

  let context = cookieSettings authParams :. jwtSettings authParams :. ghAppParams :. EmptyContext

  withRunInIO $ \run -> do
    let runBaseHandler :: BaseApp a -> Handler a
        runBaseHandler = ioToHandler . run

    Warp.run 3000 $ serveRoutes @MergeBotRoutes runBaseHandler context handleMergeBotRoutes
  where
    ioToHandler :: IO a -> Handler a
    ioToHandler m = liftIO (try m) >>= \case
      Right x -> return x
      Left e
        | Just servantErr <- fromException e -> throwError servantErr
        | otherwise -> throwError $ err500 { errBody = Char8.pack $ displayException e }

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
