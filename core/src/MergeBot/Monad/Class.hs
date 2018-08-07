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

import MergeBot.Patch (PatchId)
import MergeBot.Merge (MergeAlgorithm)

-- | Monad for manipulating branches on GitHub.
class Monad m => MonadGHBranch m where
  -- | Get the branch for the given pull request.
  getBranch :: PatchId -> m (Maybe Text)
  -- | Create the given branch based off master.
  createBranch :: Text -> m ()
  -- | Forcibly delete the given branch.
  deleteBranch :: Text -> m ()
  -- | Merge the second branch into the first branch.
  mergeBranch :: Text -> Text -> m ()

-- | Monad for manipulating pull requests on GitHub.
class Monad m => MonadGHPullRequest m where
  -- | Merge the given pull request with the given merge algorithm.
  mergePullRequest :: PatchId -> MergeAlgorithm -> m ()
