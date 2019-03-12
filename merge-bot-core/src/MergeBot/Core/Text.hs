{-|
Module      :  MergeBot.Core.Text
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines labels and messages used in the MergeBot.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Text
  ( toTryBranch
  , toTryMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Display the pull request number.
toId :: Int -> Text
toId = Text.pack . ('#':) . show

-- | Get the name of the try branch for the given pull request.
toTryBranch :: Int -> Text
toTryBranch = ("trying-" <>) . Text.pack . show

-- | Get the try commit message for the given pull request.
toTryMessage :: Int -> Text
toTryMessage prNum = Text.unwords ["Try", toId prNum]
