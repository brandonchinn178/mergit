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

type MergeBotRoutes =
  "webhook" :> WebhookRoutes
  :<|> DebugRoutes

handleMergeBotRoutes :: Server MergeBotRoutes
handleMergeBotRoutes = handleWebhookRoutes :<|> handleDebugRoutes
