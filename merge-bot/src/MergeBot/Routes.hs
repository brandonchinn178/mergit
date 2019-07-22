{-|
Module      :  MergeBot.Routes
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines all routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes
  ( MergeBotRoutes
  , handleMergeBotRoutes
  ) where

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
  -- TODO: hoist, validate token
  Authenticated token -> handleDebugRoutes $ fromUserToken token
  _ -> throwAll $ redirectToLogin authParams
