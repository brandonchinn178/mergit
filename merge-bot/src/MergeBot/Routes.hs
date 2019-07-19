{-|
Module      :  MergeBot.Routes
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines all routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes
  ( MergeBotRoutes
  , handleMergeBotRoutes
  ) where

import Servant

import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotRoutes = UnprotectedRoutes :<|> ProtectedRoutes

handleMergeBotRoutes :: Server MergeBotRoutes
handleMergeBotRoutes = handleUnprotectedRoutes :<|> handleProtectedRoutes

type UnprotectedRoutes = "webhook" :> WebhookRoutes

handleUnprotectedRoutes :: Server UnprotectedRoutes
handleUnprotectedRoutes = handleWebhookRoutes

type ProtectedRoutes = DebugRoutes

handleProtectedRoutes :: Server ProtectedRoutes
handleProtectedRoutes = handleDebugRoutes
