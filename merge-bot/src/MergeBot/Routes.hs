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

import MergeBot.Auth
    (UserToken, XsrfProtected, XsrfToken, fromUserToken, redirectToLogin)
import MergeBot.Monad (BaseApp, ServerBase, getAuthParams)
import MergeBot.Routes.Auth (AuthRoutes, handleAuthRoutes)
import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Debug.Monad (DebugApp, DebugState(..), runDebugApp)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotRoutes = UnprotectedRoutes :<|> (XsrfProtected :> Auth '[Cookie] UserToken :> ProtectedRoutes)

handleMergeBotRoutes :: ServerBase MergeBotRoutes
handleMergeBotRoutes = handleUnprotectedRoutes :<|> handleProtectedRoutes

type UnprotectedRoutes =
  "auth" :> AuthRoutes
  :<|> "webhook" :> WebhookRoutes

handleUnprotectedRoutes :: ServerBase UnprotectedRoutes
handleUnprotectedRoutes = handleAuthRoutes :<|> handleWebhookRoutes

type ProtectedRoutes = DebugRoutes

handleProtectedRoutes :: XsrfToken -> AuthResult UserToken -> ServerBase ProtectedRoutes
handleProtectedRoutes xsrfToken = \case
  Authenticated token -> hoistWith (runRoute token)
  _ -> hoistWith runRedirect
  where
    hoistWith :: (forall x. DebugApp x -> BaseApp x) -> ServerBase ProtectedRoutes
    hoistWith f = hoistServer (Proxy @ProtectedRoutes) f handleDebugRoutes

    runRoute :: UserToken -> DebugApp a -> BaseApp a
    runRoute token routeToRun = do
      let debugStateWithoutUser = DebugState
            { debugToken = fromUserToken token
            , debugXsrfToken = xsrfToken
            , debugUser = error "User is not verified"
            }

      -- make sure token isn't expired
      result <- runDebugApp debugStateWithoutUser $ githubTry' status401 $
        queryGitHub GHEndpoint
          { method = GET
          , endpoint = "/user"
          , endpointVals = []
          , ghData = []
          }

      user <- case result of
        Left _ -> redirectToLogin'
        Right (o :: Object User) -> return [get| o.login |]

      let debugState = debugStateWithoutUser { debugUser = user }
      runDebugApp debugState routeToRun

    runRedirect :: DebugApp a -> BaseApp a
    runRedirect _ = redirectToLogin'

    redirectToLogin' = redirectToLogin =<< getAuthParams
