{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

{- |
Module      :  Mergit
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the Mergit GitHub application.
-}
module Mergit (runMergit) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, fromException)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (logInfoN)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Network.Wai.Handler.Warp as Warp
import Servant (
  Application,
  Context (..),
  DefaultErrorFormatters,
  ErrorFormatters,
  Handler,
  HasContextEntry,
  HasServer,
  ServerError (..),
  ServerT,
  err500,
  hoistServerWithContext,
  serveWithContext,
  throwError,
  type (.++),
 )
import Servant.GitHub (loadGitHubAppParams)
import Text.Printf (printf)
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async (async, waitAny)
import UnliftIO.Exception (handle, try)

import Mergit.Auth (AuthParams (..), loadAuthParams)
import qualified Mergit.Core as Core
import Mergit.Core.Error (getMergitError)
import qualified Mergit.Core.GitHub as Core
import Mergit.Core.Monad (getRepo)
import Mergit.EventQueue (EventQueuesConfig (..), initEventQueuesManager)
import Mergit.Monad
import Mergit.Routes (MergitRoutes, handleMergitRoutes)

-- | Load environment variables and spin up all the threads.
runMergit :: IO ()
runMergit = do
  ghAppParams <- loadGitHubAppParams
  authParams <- loadAuthParams
  eventQueuesManager <-
    initEventQueuesManager
      EventQueuesConfig
        { globalQueueLimit = 10000
        , workerQueueLimit = 1000
        }

  runBaseApp BaseAppConfig{..} $
    concurrentlyAll
      [ runServer
      , handleBotQueue
      , pollMergeQueues
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
        logWithRepo $ printf "PR created: %d" prNum
        Core.createCheckRuns sha
      CommitPushedToPR prNum sha -> do
        logWithRepo $ printf "Commit pushed to PR: %d" prNum
        Core.createCheckRuns sha
      StartTryJob prNum sha branch -> do
        logWithRepo $ printf "Trying PR #%d for commit: %s" prNum sha
        Core.startTryJob prNum sha branch
      QueuePR prNum sha -> do
        logWithRepo $ printf "Queuing PR #%d" prNum
        Core.queuePR prNum sha
      DequeuePR prNum sha -> do
        logWithRepo $ printf "Dequeuing PR #%d" prNum
        Core.dequeuePR prNum sha
      ResetMerge prNum sha -> do
        logWithRepo $ printf "Resetting merge check run for PR #%d" prNum
        Core.resetMerge prNum sha
      RefreshCheckRun branch sha -> do
        logWithRepo $ printf "Handling status update for %s (%s)" branch sha
        Core.handleStatusUpdate branch sha
      DeleteBranch branch -> do
        logWithRepo $ printf "Deleting branch: %s" branch
        Core.deleteBranch branch
      PollQueues -> do
        logWithRepo "Polling queues"
        Core.pollQueues

    logWithRepo msg = do
      (repoOwner, repoName) <- getRepo
      logInfoN $ Text.pack $ printf "[%s/%s] %s" repoOwner repoName (msg :: String)

    -- TODO: calling getRepoAndTokens for each event is very inefficient, maybe using some token pool?
    getTokenForRepo repo = do
      mToken <- lookup repo <$> getRepoAndTokens
      maybe (error $ "Could not find repository: " ++ show repo) return mToken

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

    Warp.run 3000 $ serveRoutes @MergitRoutes runBaseHandler context handleMergitRoutes
  where
    ioToHandler :: IO a -> Handler a
    ioToHandler m =
      liftIO (try m) >>= \case
        Right x -> return x
        Left e ->
          throwError $
            if
                | Just servantErr <- fromException e -> servantErr
                | Just mergitError <- fromException e -> to500 $ getMergitError mergitError
                | otherwise -> to500 $ Text.pack $ displayException e

    to500 msg = err500{errBody = TextL.encodeUtf8 $ TextL.fromStrict msg}

{- Helpers -}

logException :: MonadIO m => SomeException -> m ()
logException = liftIO . putStrLn . displayException

{- | Run each of the given actions in a separate thread.

 If any action throws an exception, rethrow the exception.
-}
concurrentlyAll :: MonadUnliftIO m => [m ()] -> m ()
concurrentlyAll actions = do
  threads <- mapM async actions
  void $ waitAny threads

serveRoutes ::
  forall api context m.
  ( HasServer api context
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  (forall x. m x -> Handler x) ->
  Context context ->
  ServerT api m ->
  Application
serveRoutes f context routes =
  serveWithContext (Proxy @api) context $
    hoistServerWithContext (Proxy @api) (Proxy @context) f routes
