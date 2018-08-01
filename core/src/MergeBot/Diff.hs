{-|
Module      :  MergeBot.Diff
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Diff data type, representing a GitHub pull request.

Note on naming: a pull request can be thought of as a code diff
trying to be merged into the main codebase.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Diff
  ( DiffId
  , DiffOptions(..)
  , DiffOptionsPartial
  , DiffOptionsFull
  , resolveOptions
  ) where

import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)

import MergeBot.Merge (MergeAlgorithm)

-- | The ID of a GitHub pull request.
type DiffId = Int

-- | Options specified per-PR to customize merge bot behavior.
data DiffOptions f = DiffOptions
  { defaultMergeAlgorithm :: HKD f MergeAlgorithm
  }

-- | Strip out Identity applications.
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

type DiffOptionsPartial = DiffOptions Maybe
deriving instance Show DiffOptionsPartial
deriving instance Eq DiffOptionsPartial

type DiffOptionsFull = DiffOptions Identity
deriving instance Show DiffOptionsFull
deriving instance Eq DiffOptionsFull

-- | Resolve the given options using the given defaults.
resolveOptions :: DiffOptionsPartial -> DiffOptionsFull -> DiffOptionsFull
resolveOptions partial full = DiffOptions
  { defaultMergeAlgorithm = fromMaybe (defaultMergeAlgorithm full) (defaultMergeAlgorithm partial)
  }
