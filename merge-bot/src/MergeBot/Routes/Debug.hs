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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Debug
  ( DebugRoutes
  , handleDebugRoutes
  ) where

import Control.Monad (forM, forM_)
import Data.Aeson.Schema (Object, get, schema)
import GitHub.REST (GHEndpoint(..), KeyValue(..), StdMethod(..), queryGitHub)
import GitHub.Schema.Repository (Repository)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import MergeBot.Monad (getInstallations)
import MergeBot.Routes.Debug.Monad (DebugApp, ServerDebug, getUser, liftBaseApp)

type DebugRoutes = IndexPage

handleDebugRoutes :: ServerDebug DebugRoutes
handleDebugRoutes = handleIndexPage

{- Index page -}

type IndexPage = HtmlPage
handleIndexPage :: DebugApp Html
handleIndexPage = do
  installations <- liftBaseApp getInstallations

  repositories <- fmap concat $ forM installations $ \installationId ->
    [get| .repositories |] <$>
      queryGitHub @_ @(Object [schema| { repositories: List #Repository } |]) GHEndpoint
        { method = GET
        , endpoint = "/user/installations/:installation_id/repositories"
        , endpointVals = [ "installation_id" := installationId ]
        , ghData = []
        }

  render $ do
    H.h2 "Available repositories"
    forM_ repositories $ \repo ->
      H.li $ H.a ! A.href "#" $ H.toHtml [get| repo.full_name |]

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
