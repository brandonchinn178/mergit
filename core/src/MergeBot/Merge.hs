{-|
Module      :  MergeBot.Merge
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the merging capabilities of the merge bot.
-}

module MergeBot.Merge
  ( MergeAlgorithm(..)
  ) where

-- | The merging algorithm to run when a PR is to be upstreamed to master.
data MergeAlgorithm = Merge | Squash
  deriving (Show,Eq)
