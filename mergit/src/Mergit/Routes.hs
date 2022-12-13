{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      :  Mergit.Routes
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines all routes for Mergit.
-}
module Mergit.Routes (
  MergitRoutes,
  handleMergitRoutes,
) where

import Data.Aeson.Schema (Object, get)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import GitHub.REST (GHEndpoint (..), githubTry', queryGitHub)
import GitHub.Schema.User (User)
import Network.HTTP.Types (status401)
import Servant
import Servant.Auth.Server (Auth, AuthResult (..), Cookie)

import Mergit.Auth (
  UserToken,
  XsrfProtected,
  XsrfToken,
  fromUserToken,
  redirectToLogin,
 )
import Mergit.Monad (BaseApp, ServerBase, getAuthParams)
import Mergit.Routes.Auth (AuthRoutes, handleAuthRoutes)
import Mergit.Routes.Debug (DebugRoutes, handleDebugRoutes)
import Mergit.Routes.Debug.Monad (DebugApp, DebugState (..), runDebugApp)
import Mergit.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergitRoutes = UnprotectedRoutes :<|> (XsrfProtected :> Auth '[Cookie] UserToken :> ProtectedRoutes)

handleMergitRoutes :: ServerBase MergitRoutes
handleMergitRoutes = handleUnprotectedRoutes :<|> handleProtectedRoutes

type UnprotectedRoutes =
  "auth" :> AuthRoutes
    :<|> "webhook" :> WebhookRoutes
    :<|> "static" :> Raw

handleUnprotectedRoutes :: ServerBase UnprotectedRoutes
handleUnprotectedRoutes = handleAuthRoutes :<|> handleWebhookRoutes :<|> serveStaticFiles

serveStaticFiles :: ServerBase Raw
serveStaticFiles =
  serveDirectoryEmbedded
    [ ("logo.svg", $(makeRelativeToProject "../assets/logo.svg" >>= embedFile))
    ]

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
      let debugStateWithoutUser =
            DebugState
              { debugToken = fromUserToken token
              , debugXsrfToken = xsrfToken
              , debugUser = error "User is not verified"
              }

      -- make sure token isn't expired
      result <-
        runDebugApp debugStateWithoutUser $
          githubTry' status401 $
            queryGitHub
              GHEndpoint
                { method = GET
                , endpoint = "/user"
                , endpointVals = []
                , ghData = []
                }

      user <- case result of
        Left _ -> redirectToLogin'
        Right (o :: Object User) -> return [get| o.login |]

      let debugState = debugStateWithoutUser{debugUser = user}
      runDebugApp debugState routeToRun

    runRedirect :: DebugApp a -> BaseApp a
    runRedirect _ = redirectToLogin'

    redirectToLogin' = redirectToLogin =<< getAuthParams
