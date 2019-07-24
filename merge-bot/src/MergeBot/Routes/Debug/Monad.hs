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
import Servant (Handler, ServantErr, ServerT)
import Servant.GitHub (GitHubAppParams(..), loadGitHubAppParams)
import UnliftIO (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import UnliftIO.Exception (catch, throwIO)

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Monad (BotApp, runBotApp, runIO)

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
              IO
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

runDebugApp :: Token -> DebugApp a -> Handler a
runDebugApp token action = do
  GitHubAppParams{ghUserAgent} <- liftIO loadGitHubAppParams

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

  runIO
    . runQueryT graphqlSettings
    . runGitHubT ghState
    . (`runReaderT` debugState)
    . unDebugApp
    $ action

runBotAppDebug :: Text -> BotApp a -> DebugApp a
runBotAppDebug repo action = do
  token <- DebugApp $ asks debugToken
  liftIO $ runBotApp repo action token

-- | Get the currently authenticated user.
getUser :: DebugApp Text
getUser = DebugApp $ fromMaybe "Anonymous" <$> asks debugUser

withUser :: Text -> DebugApp a -> DebugApp a
withUser user = DebugApp . local (\state -> state { debugUser = Just user }) . unDebugApp
