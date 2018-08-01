{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Monads used in the core library.
-}

module MergeBot.Monad
  ( MonadGHPromote(..)
  ) where

-- | Monad for promoting the staging branch to master.
class Monad m => MonadGHPromote m where
  -- | Check if staging can be merged into master safely.
  canPromoteStaging :: m Bool
  -- | Set `master` to the HEAD of `staging`.
  promoteStaging :: m ()
