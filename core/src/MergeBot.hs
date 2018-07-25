{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}

module MergeBot
  ( PR(..)
  , PROption(..)
  , BotState(..)
  ) where

import Data.Set (Set)

import MergeBot.Merge (MergeAlgorithm)

-- | The internal representation of a GitHub PR.
data PR = PR
  { prID   :: Int
  , prOpts :: Set PROption
  } deriving (Show)

-- | Options specified per-PR to customize merge bot behavior.
data PROption
  = MergeAlgorithm MergeAlgorithm
  deriving (Show)

-- | The state of the merge bot.
data BotState = BotState
  { mergeQueue :: [PR]
  , mergeJobs  :: [PR]
  , tryJobs    :: [PR]
  } deriving (Show)
