{-|
Module      :  MergeBot.Core.Patch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Patch data type, representing a GitHub pull request.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Patch
  ( Patch(..)
  , PatchId
  ) where

import Data.Text (Text)

-- | A GitHub pull request.
data Patch = Patch
  { patchId   :: PatchId
  , patchName :: Text
  } deriving (Show,Eq)

-- | The ID of a GitHub pull request.
type PatchId = Int
