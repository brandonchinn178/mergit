{-|
Module      :  MergeBot.Core.Status
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for resolving CI statuses.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module MergeBot.Core.Status
  ( CIStatus
  , getCIStatus
  , displayCIStatus
  ) where

import Data.Aeson.Schema (get)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.StatusState (StatusState)
import qualified GitHub.Data.StatusState as StatusState

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (CIContext)

type CIStatus = [(Text, (StatusState, Maybe Text))]

-- | Get CI statuses compiled from the merge bot config and the contexts for a commit.
--
-- Returns a map from context name to the state of the context and the associated URL.
getCIStatus :: BotConfig -> [CIContext] -> CIStatus
getCIStatus BotConfig{requiredStatuses} = fromStatusMap . foldl updateStatusMap empty
  where
    empty = HashMap.fromList . map (, (StatusState.EXPECTED, Nothing)) $ requiredStatuses
    updateStatusMap statuses context = HashMap.adjust
      (const [get| context.(state, targetUrl) |])
      [get| context.context |]
      statuses
    fromStatusMap statuses =
      -- iterate on requiredStatuses to keep order
      map (\context -> (context, statuses HashMap.! context)) requiredStatuses

-- | Get text containing Markdown to display the given CIStatus.
displayCIStatus :: CIStatus -> Text
displayCIStatus status = Text.unlines $ header ++ map (uncurry mkLine) status
  where
    header =
      [ "CI Job | Status"
      , ":-----:|:-----:"
      ]
    mkLine context (state, url) =
      let emoji = case state of
            StatusState.ERROR    -> "❗"
            StatusState.EXPECTED -> "💤"
            StatusState.FAILURE  -> "❌"
            StatusState.PENDING  -> "⏳"
            StatusState.SUCCESS  -> "✅"
          link = case url of
            Nothing -> context
            Just url' -> "[" <> context <> "](" <> url' <> ")"
      in link <> " | " <> emoji
