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

import MergeBot.Merge (MergeAlgorithm)
import MergeBot.Patch (Patch, PatchId)

-- | Monad for manipulating branches on GitHub.
class Monad m => MonadGHBranch m where
  -- | Get the branch for the given pull request.
  getBranch :: PatchId -> m (Maybe Text)
  -- | Create the given branch based off master.
  createBranch :: Text -> m ()
  -- | Forcibly delete the given branch, if it exists.
  deleteBranch :: Text -> m ()
  -- | Merge the second branch into the first branch.
  mergeBranch :: Text -> Text -> m ()

-- | Monad for manipulating pull requests on GitHub.
class Monad m => MonadGHPullRequest m where
  -- | Return all open pull requests.
  listPullRequests :: m [Patch]
  -- | Return True if a pull request is approved by all reviewers.
  isApproved :: PatchId -> m Bool
  -- | Post a comment to a pull request.
  postComment :: PatchId -> Text -> m ()
  -- | Merge the given pull request with the given merge algorithm.
  mergePullRequest :: PatchId -> MergeAlgorithm -> m ()
