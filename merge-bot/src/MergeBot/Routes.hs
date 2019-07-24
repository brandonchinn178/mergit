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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes
  ( MergeBotRoutes
  , handleMergeBotRoutes
  ) where

import Data.Aeson.Schema (Object, get)
import GitHub.REST (GHEndpoint(..), githubTry', queryGitHub)
import GitHub.Schema.User (User)
import Network.HTTP.Types (StdMethod(..), status401)
import Servant
import Servant.Auth.Server (Auth, AuthResult(..), Cookie)

import MergeBot.Auth (UserToken, fromUserToken, redirectToLogin)
import MergeBot.Monad (BaseApp, ServerBase, getAuthParams)
import MergeBot.Routes.Auth (AuthRoutes, handleAuthRoutes)
import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Debug.Monad (DebugApp, runDebugApp, withUser)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotRoutes = UnprotectedRoutes :<|> (Auth '[Cookie] UserToken :> ProtectedRoutes)

handleMergeBotRoutes :: ServerBase MergeBotRoutes
handleMergeBotRoutes = handleUnprotectedRoutes :<|> handleProtectedRoutes

type UnprotectedRoutes =
  "auth" :> AuthRoutes
  :<|> "webhook" :> WebhookRoutes

handleUnprotectedRoutes :: ServerBase UnprotectedRoutes
handleUnprotectedRoutes = handleAuthRoutes :<|> handleWebhookRoutes

type ProtectedRoutes = DebugRoutes

handleProtectedRoutes :: AuthResult UserToken -> ServerBase ProtectedRoutes
handleProtectedRoutes = \case
  Authenticated token -> hoistWith (runRoute token)
  _ -> hoistWith runRedirect
  where
    hoistWith :: (forall x. DebugApp x -> BaseApp x) -> ServerBase ProtectedRoutes
    hoistWith f = hoistServer (Proxy @ProtectedRoutes) f handleDebugRoutes

    runRoute :: UserToken -> DebugApp a -> BaseApp a
    runRoute token routeToRun = do
      authParams <- getAuthParams

      runDebugApp (fromUserToken token) $ do
        -- make sure token isn't expired
        result <- githubTry' status401 $ queryGitHub GHEndpoint
          { method = GET
          , endpoint = "/user"
          , endpointVals = []
          , ghData = []
          }

        user <- case result of
          Left _ -> redirectToLogin authParams
          Right (o :: Object User) -> return [get| o.login |]

        withUser user routeToRun

    runRedirect :: DebugApp a -> BaseApp a
    runRedirect _ = redirectToLogin =<< getAuthParams
