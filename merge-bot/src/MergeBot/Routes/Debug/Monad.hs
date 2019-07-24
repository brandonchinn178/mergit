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
  , runDebugApp
  , runBotAppDebug
  , getUser
  , withUser
  , liftBaseApp
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, lift, local, runReaderT)
import Data.GraphQL (QuerySettings(..), QueryT, defaultQuerySettings, runQueryT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GitHub.REST (GitHubState(..), GitHubT, MonadGitHubREST(..), runGitHubT)
import GitHub.REST.Auth (Token, fromToken)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)
import Servant (ServantErr, ServerT)
import Servant.GitHub (GitHubAppParams(..))
import UnliftIO (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import UnliftIO.Exception (catch, throwIO)

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Monad (BaseApp, BotApp, getGitHubAppParams, runBotApp)

type ServerDebug api = ServerT api DebugApp

data DebugState = DebugState
  { debugToken :: Token
  , debugUser  :: Maybe Text
  }

newtype DebugApp a = DebugApp
  { unDebugApp ::
      ReaderT DebugState
        ( GitHubT
          ( QueryT API
              BaseApp
          )
        )
        a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadGitHubREST DebugApp where
  queryGitHub = DebugApp . lift . queryGitHub

instance MonadError ServantErr DebugApp where
  throwError = throwIO
  catchError = catch

instance MonadUnliftIO DebugApp where
  askUnliftIO = DebugApp $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unDebugApp)

runDebugApp :: Token -> DebugApp a -> BaseApp a
runDebugApp token action = do
  GitHubAppParams{ghUserAgent} <- getGitHubAppParams

  let ghState = GitHubState { token, userAgent = ghUserAgent, apiVersion = "antiope-preview" }
      graphqlSettings :: QuerySettings API
      graphqlSettings = defaultQuerySettings
        { url = "https://api.github.com/graphql"
        , modifyReq = \req -> req
          { requestHeaders =
              (hAuthorization, fromToken token)
              : (hUserAgent, ghUserAgent)
              : (hAccept, "application/vnd.github.antiope-preview+json")
              : requestHeaders req
          }
        }
      debugState = DebugState
        { debugToken = token
        , debugUser = Nothing
        }

  runQueryT graphqlSettings
    . runGitHubT ghState
    . (`runReaderT` debugState)
    . unDebugApp
    $ action

runBotAppDebug :: Text -> BotApp a -> DebugApp a
runBotAppDebug repo action = do
  token <- DebugApp $ asks debugToken
  liftBaseApp $ runBotApp repo action token

-- | Get the currently authenticated user.
getUser :: DebugApp Text
getUser = DebugApp $ fromMaybe "Anonymous" <$> asks debugUser

withUser :: Text -> DebugApp a -> DebugApp a
withUser user = DebugApp . local (\state -> state { debugUser = Just user }) . unDebugApp

liftBaseApp :: BaseApp a -> DebugApp a
liftBaseApp = DebugApp . lift . lift . lift
