{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy(..))
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
import MergeBot.Monad
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)

-- | Load environment variables and spin up all the merge bot threads.
runMergeBot :: IO ()
runMergeBot = concurrentlyAllIO
  [ pollMergeQueues
  , runServer
  ]
  where
    concurrentlyAllIO :: [BaseApp ()] -> IO ()
    concurrentlyAllIO actions = do
      ghAppParams <- loadGitHubAppParams
      authParams <- loadAuthParams
      runBaseApp ghAppParams authParams $ concurrentlyAll actions

-- TODO: instead of polling, have the completed merge run start the next merge run
pollMergeQueues :: BaseApp ()
pollMergeQueues = forever $ do
  handle logException $ void $ runBotAppOnAllRepos Core.pollQueues
  liftIO $ threadDelay $ pollDelayMinutes * 60e6
  where
    logException = liftIO . putStrLn . displayException @SomeException
    pollDelayMinutes = 10

runServer :: BaseApp ()
runServer = do
  ghAppParams <- getGitHubAppParams
  authParams <- getAuthParams

  let runBaseHandler' = runBaseHandler ghAppParams authParams
      context = cookieSettings authParams :. jwtSettings authParams :. ghAppParams :. EmptyContext

  liftIO $ Warp.run 3000 $ serveRoutes @MergeBotRoutes runBaseHandler' context handleMergeBotRoutes

{- Helpers -}

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
