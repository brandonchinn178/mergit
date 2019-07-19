{-|
Module      :  MergeBot.Routes.Debug
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines debugging routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Debug
  ( DebugRoutes
  , handleDebugRoutes
  ) where

import GitHub.REST (Token)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (ToMarkup)
import qualified Text.Blaze.Html5 as H

type DebugRoutes = Get '[HTML] IndexPage

handleDebugRoutes :: Token -> Server DebugRoutes
handleDebugRoutes token = handleIndexPage token

{- Index page -}

data IndexPage = IndexPage

instance ToMarkup IndexPage where
  toMarkup IndexPage = H.html $ do
    H.head $
      H.title "LY Merge Bot"
    H.body $ do
      H.p "Hello world"

handleIndexPage :: Token -> Handler IndexPage
handleIndexPage _ = return IndexPage
