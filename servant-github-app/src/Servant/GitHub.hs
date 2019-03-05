{-|
Module      :  Servant.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Servant combinators for serving a GitHub App.
-}

module Servant.GitHub
  ( GitHubSigned
  , GitHubEvent
  , loadGitHubAppParams
  , module Servant.GitHub.Event
  -- * Re-exports
  , Object
  ) where

import Data.Aeson.Schema (Object)

import Servant.GitHub.Combinators
import Servant.GitHub.Context
import Servant.GitHub.Event
