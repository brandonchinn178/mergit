{-|
Module      :  MergeBot.Monad.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines monad definitions for the GitHub API.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MergeBot.Monad.GitHub
  ( GitHubT(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))

import MergeBot.Monad.Class

newtype GitHubT m a = GitHubT { runGitHubT :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadGHBranch (GitHubT m) where
  createBranch _ = GitHubT $ liftIO $ putStrLn "createBranch"
  deleteBranch _ = GitHubT $ liftIO $ putStrLn "deleteBranch"
  mergeBranches _ _ = GitHubT $ liftIO $ putStrLn "mergeBranches"

instance MonadIO m => MonadGHPullRequest (GitHubT m) where
  mergePullRequest _ _ = GitHubT $ liftIO $ putStrLn "mergePullRequest"
