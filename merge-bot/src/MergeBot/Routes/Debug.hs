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
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

import MergeBot.Monad (DebugApp, ServerDebug, getUser)

type DebugRoutes = IndexPage

handleDebugRoutes :: ServerDebug DebugRoutes
handleDebugRoutes = handleIndexPage

{- Index page -}

type IndexPage = HtmlPage
handleIndexPage :: DebugApp Html
handleIndexPage = render "Hello world"

{- Helpers -}

type HtmlPage = Get '[HTML] Html

-- | Renders the given body within the general template.
render :: Html -> DebugApp Html
render body = do
  user <- getUser
  return $ H.html $ do
    H.head $
      H.title "LeapYear Merge Bot"
    H.body $ do
      H.header $ do
        H.h1 "LeapYear Merge Bot"
        H.p $ do
          "Logged in as: "
          H.strong $ H.toHtml user
      H.main body
