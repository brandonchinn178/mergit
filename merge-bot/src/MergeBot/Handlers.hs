{-|
Module      :  MergeBot.Handlers
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines handlers for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}

module MergeBot.Handlers
  ( handleInstallation
  , handlePullRequest
  ) where

import Data.Aeson.Schema (Object)
import GitHub.REST (Token)
import Servant
import qualified Servant.GitHub as GitHub

-- | Handle the 'installation' GitHub event.
handleInstallation :: Object GitHub.InstallationSchema -> Token -> Handler ()
handleInstallation = undefined

-- | Handle the 'pull_request' GitHub event.
handlePullRequest :: Object GitHub.PullRequestSchema -> Token -> Handler ()
handlePullRequest = undefined
