{-|
Module      :  MergeBot.Routes.Debug
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines debugging routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Debug
  ( DebugRoutes
  , handleDebugRoutes
  ) where

import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (ToMarkup)
import qualified Text.Blaze.Html5 as H

import MergeBot.Monad (DebugApp, ServerDebug)

type DebugRoutes = Get '[HTML] IndexPage

handleDebugRoutes :: ServerDebug DebugRoutes
handleDebugRoutes = handleIndexPage

{- Index page -}

data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage = H.html $ do
    H.head $
      H.title "LY Merge Bot"
    H.body $ do
      H.p "Hello world"

handleIndexPage :: DebugApp IndexPage
handleIndexPage = return IndexPage
