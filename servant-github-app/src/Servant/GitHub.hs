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
  , WithToken
  , WithToken'
  , TokenType(..)
  , GitHubAction
  , GitHubAppParams(..)
  , loadGitHubAppParams
  , module Servant.GitHub.Event
  -- * Re-exports
  , Object
  , Token
  ) where

import Data.Aeson.Schema (Object)
import GitHub.REST (Token)

import Servant.GitHub.Combinators
import Servant.GitHub.Context
import Servant.GitHub.Event
