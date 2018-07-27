{-|
Module      :  MergeBot.Diff
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Diff data type, representing a GitHub pull request.

Note on naming: a pull request can be thought of as a code diff
trying to be merged into the main codebase.
-}

module MergeBot.Diff
  ( DiffId
  , DiffOption(..)
  ) where

import MergeBot.Merge (MergeAlgorithm)

-- | The ID of a GitHub pull request.
type DiffId = Int

-- | Options specified per-PR to customize merge bot behavior.
data DiffOption
  = MergeAlgorithm MergeAlgorithm
  deriving (Show,Eq)
