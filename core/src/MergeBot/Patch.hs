{-|
Module      :  MergeBot.Patch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Patch data type, representing a GitHub pull request.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Patch
  ( PatchId
  , PatchOptions(..)
  , PatchOptionsPartial
  , PatchOptionsFull
  , resolveOptions
  ) where

import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)

import MergeBot.Merge (MergeAlgorithm)

-- | The ID of a GitHub pull request.
type PatchId = Int

-- | Options specified per-PR to customize merge bot behavior.
data PatchOptions f = PatchOptions
  { mergeAlgorithm :: HKD f MergeAlgorithm
  }

-- | Strip out Identity applications.
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

type PatchOptionsPartial = PatchOptions Maybe
deriving instance Show PatchOptionsPartial
deriving instance Eq PatchOptionsPartial

type PatchOptionsFull = PatchOptions Identity
deriving instance Show PatchOptionsFull
deriving instance Eq PatchOptionsFull

-- | Resolve the given options using the given defaults.
resolveOptions :: PatchOptionsPartial -> PatchOptionsFull -> PatchOptionsFull
resolveOptions partial full = PatchOptions
  { mergeAlgorithm = fromMaybe (mergeAlgorithm full) (mergeAlgorithm partial)
  }
