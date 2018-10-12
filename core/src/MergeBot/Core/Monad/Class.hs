{-|
Module      :  MergeBot.Core.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Monad typeclasses used in the core library.
-}

module MergeBot.Core.Monad.Class
  ( MonadGithub(..)
  ) where

import MergeBot.Core.Data

class Monad m => MonadGithub m where
  -- | List all open pull requests.
  listPullRequests :: m [PullRequest]
