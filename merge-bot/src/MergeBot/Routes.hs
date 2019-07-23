{-|
Module      :  MergeBot.Routes
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines all routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes
  ( MergeBotRoutes
  , handleMergeBotRoutes
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import GitHub.REST
    ( GHEndpoint(..)
    , GitHubState(..)
    , Token
    , githubTry'
    , queryGitHub
    , runGitHubT
    )
import Network.HTTP.Types (StdMethod(..), status401)
import Servant
import Servant.Auth.Server (Auth, AuthResult(..), Cookie, throwAll)

import MergeBot.Routes.Auth
    ( AuthParams
    , AuthRoutes
    , UserToken
    , fromUserToken
    , handleAuthRoutes
    , redirectToLogin
    )
import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotRoutes = UnprotectedRoutes :<|> (Auth '[Cookie] UserToken :> ProtectedRoutes)

handleMergeBotRoutes :: AuthParams -> Server MergeBotRoutes
handleMergeBotRoutes authParams = handleUnprotectedRoutes authParams :<|> handleProtectedRoutes authParams

type UnprotectedRoutes =
  "auth" :> AuthRoutes
  :<|> "webhook" :> WebhookRoutes

handleUnprotectedRoutes :: AuthParams -> Server UnprotectedRoutes
handleUnprotectedRoutes authParams = handleAuthRoutes authParams :<|> handleWebhookRoutes

type ProtectedRoutes = DebugRoutes

handleProtectedRoutes :: AuthParams -> AuthResult UserToken -> Server ProtectedRoutes
handleProtectedRoutes authParams = \case
  Authenticated token -> hoistServer (Proxy @ProtectedRoutes) (runRoute $ fromUserToken token) $
    handleDebugRoutes (fromUserToken token)
  _ -> throwAll $ redirectToLogin authParams
  where
    -- TODO: first `Handler a` should be a custom monad
    runRoute :: Token -> Handler a -> Handler a
    runRoute token routeToRun = do
      let ghState = GitHubState{token, userAgent = "LeapYear/merge-bot", apiVersion = "v3"}
      result <- liftIO $ runGitHubT ghState $
        githubTry' status401 $ queryGitHub GHEndpoint
          { method = GET
          , endpoint = "/user"
          , endpointVals = []
          , ghData = []
          }

      case result of
        Left _ -> throwError $ redirectToLogin authParams
        Right (_ :: Value) -> return ()

      routeToRun
