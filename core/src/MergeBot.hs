{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot
  ( module MergeBot
  ) where

import MergeBot.Config as MergeBot
import MergeBot.Diff as MergeBot
import MergeBot.Merge as MergeBot
import MergeBot.State as MergeBot
