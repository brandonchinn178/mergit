{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Monads used in the core library.
-}

module MergeBot.Monad
  ( MonadGHBranch(..)
  , MonadGHPullRequest(..)
  , MonadGHPromote(..)
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

-- | Monad for promoting the staging branch to master.
class Monad m => MonadGHPromote m where
  -- | Check if staging can be merged into master safely.
  canPromoteStaging :: m Bool
  -- | Set `master` to the HEAD of `staging`.
  promoteStaging :: m ()
