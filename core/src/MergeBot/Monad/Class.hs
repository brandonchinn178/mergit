{-|
Module      :  MergeBot.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Monad typeclasses used in the core library.
-}

module MergeBot.Monad.Class
  ( MonadGHBranch(..)
  , MonadGHPullRequest(..)
  ) where

import Data.Text (Text)

import MergeBot.Diff (DiffId)
import MergeBot.Merge (MergeAlgorithm)

-- | Monad for manipulating branches on GitHub.
class Monad m => MonadGHBranch m where
  -- | Create the given branch based off master.
  createBranch :: Text -> m ()
  -- | Forcibly delete the given branch.
  deleteBranch :: Text -> m ()
  -- | Merge the given pull requests into the given branch.
  mergeBranches :: Text -> [DiffId] -> m ()

-- | Monad for manipulating pull requests on GitHub.
class Monad m => MonadGHPullRequest m where
  -- | Merge the given pull request with the given merge algorithm.
  mergePullRequest :: DiffId -> MergeAlgorithm -> m ()
