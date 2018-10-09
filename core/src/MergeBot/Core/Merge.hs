{-|
Module      :  MergeBot.Core.Merge
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the merging capabilities of the merge bot.
-}

module MergeBot.Core.Merge
  ( MergeAlgorithm(..)
  ) where

-- | The merging algorithm to run when a PR is to be upstreamed to master.
data MergeAlgorithm = Merge | Squash
  deriving (Eq)

instance Show MergeAlgorithm where
  -- See https://developer.github.com/v3/pulls/#merge-a-pull-request-merge-button
  show Merge = "merge"
  show Squash = "squash"
