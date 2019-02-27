{-|
Module      :  Servant.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Servant combinators for serving a GitHub App.
-}

module Servant.GitHub
  ( GitHubSigned
  , loadGitHubAppParams
  ) where

import Servant.GitHub.Combinators
import Servant.GitHub.Context
