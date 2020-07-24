{-|
Module      :  MergeBot.Routes.Debug.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the monad for running debug routes.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Routes.Debug.Monad
  ( ServerDebug
  , DebugApp
  , DebugState(..)
  , runDebugApp
  , runBotAppDebug
  , getUser
  , liftBaseApp
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Text (Text)
import GitHub.REST (GitHubState(..), GitHubT, MonadGitHubREST(..), runGitHubT)
import GitHub.REST.Auth (Token)
import Servant (ServantErr, ServerT)
import Servant.GitHub (GitHubAppParams(..))
import UnliftIO (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import UnliftIO.Exception (catch, throwIO)

import MergeBot.Monad (BaseApp, BotApp, getGitHubAppParams, runBotApp)

type ServerDebug api = ServerT api DebugApp

data DebugState = DebugState
  { debugToken :: Token
  , debugUser  :: Text
  }

newtype DebugApp a = DebugApp
  { unDebugApp :: ReaderT DebugState (GitHubT BaseApp) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadGitHubREST DebugApp where
  queryGitHubPage' = DebugApp . lift . queryGitHubPage'

instance MonadError ServantErr DebugApp where
  throwError = throwIO
  catchError = catch

instance MonadUnliftIO DebugApp where
  askUnliftIO = DebugApp $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unDebugApp)

runDebugApp :: DebugState -> DebugApp a -> BaseApp a
runDebugApp debugState action = do
  GitHubAppParams{ghUserAgent} <- getGitHubAppParams

  let ghState = GitHubState
        { token = Just $ debugToken debugState
        , userAgent = ghUserAgent
        , apiVersion = "machine-man-preview"
        }

  runGitHubT ghState
    . (`runReaderT` debugState)
    . unDebugApp
    $ action

runBotAppDebug :: Text -> Text -> BotApp a -> DebugApp a
runBotAppDebug repoOwner repoName action = do
  token <- DebugApp $ asks debugToken
  liftBaseApp $ runBotApp repoOwner repoName action token

-- | Get the currently authenticated user.
getUser :: DebugApp Text
getUser = DebugApp $ asks debugUser

liftBaseApp :: BaseApp a -> DebugApp a
liftBaseApp = DebugApp . lift . lift
