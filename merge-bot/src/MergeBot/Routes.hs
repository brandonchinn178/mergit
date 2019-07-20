{-|
Module      :  MergeBot.Routes
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines all routes for the MergeBot.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes
  ( MergeBotRoutes
  , handleMergeBotRoutes
  ) where

import Servant
import Servant.Auth.Server (Auth, AuthResult(..), Cookie, throwAll)

import MergeBot.Routes.Auth
    (AuthRoutes, UserToken, fromUserToken, handleAuthRoutes, redirectToLogin)
import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotRoutes = UnprotectedRoutes :<|> (Auth '[Cookie] UserToken :> ProtectedRoutes)

handleMergeBotRoutes :: Server MergeBotRoutes
handleMergeBotRoutes = handleUnprotectedRoutes :<|> handleProtectedRoutes

type UnprotectedRoutes =
  "auth" :> AuthRoutes
  :<|> "webhook" :> WebhookRoutes

handleUnprotectedRoutes :: Server UnprotectedRoutes
handleUnprotectedRoutes = handleAuthRoutes :<|> handleWebhookRoutes

type ProtectedRoutes = DebugRoutes

handleProtectedRoutes :: AuthResult UserToken -> Server ProtectedRoutes
handleProtectedRoutes = \case
  -- TODO: hoist, validate token
  Authenticated token -> handleDebugRoutes $ fromUserToken token
  _ -> throwAll redirectToLogin
