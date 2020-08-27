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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Core.Status
  ( getCIStatus
  , resolveCIStatus
  , displayCIStatus
  ) where

import Data.Aeson.Schema (get, unwrap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GitHub.Data.URI as URI

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (CIContext)
import MergeBot.Core.GraphQL.Enums.StatusState (StatusState)
import qualified MergeBot.Core.GraphQL.Enums.StatusState as StatusState

type CIContextInfo = [unwrap| CIContext.(context, state, targetUrl) |]

data CIStatus = CIStatus
  { ciContexts      :: [CIContextInfo]
    -- ^ Only contains contexts in the merge bot config, in the same order as in the config
  , ciErrorContexts :: [CIContextInfo]
    -- ^ If the CI didn't even start; e.g. submitting an invalid configuration file to Circle CI
  } deriving (Show)

-- | Get CI statuses compiled from the merge bot config and the contexts for a commit.
--
-- Returns a map from context name to the state of the context and the associated URL.
getCIStatus :: BotConfig -> [CIContext] -> CIStatus
getCIStatus BotConfig{requiredStatuses} contexts =
  let contextNames = map [get| .context |] contexts
      statusMap = HashMap.fromList $ zip contextNames $ map [get| .(state, targetUrl) |] contexts
  in CIStatus
    { ciContexts = mapMaybe (lookupStatusMap True statusMap) requiredStatuses
    , ciErrorContexts = mapMaybe (lookupStatusMap False statusMap) errorContextNames
    }
  where
    lookupStatusMap withDefault statusMap context =
      case HashMap.lookup context statusMap of
        Nothing -> if withDefault then Just (context, StatusState.EXPECTED, Nothing) else Nothing
        Just (state, url) -> Just (context, state, url)
    -- contexts to look for to fail the entire check run
    errorContextNames =
      [ "ci/circleci: Build Error"
      ]

-- | Resolve the given CIStatus as a single overall StatusState.
resolveCIStatus :: CIStatus -> StatusState
resolveCIStatus CIStatus{..} =
  if null ciErrorContexts
    then summarizeStatuses $ map (\(_, state, _) -> state) ciContexts
    else StatusState.ERROR

-- | Get text containing Markdown to display the given CIStatus.
displayCIStatus :: CIStatus -> Text
displayCIStatus CIStatus{..} = Text.unlines $ header ++ map mkLine (ciContexts ++ ciErrorContexts)
  where
    header =
      [ "CI Job | Status"
      , ":-----:|:-----:"
      ]
    mkLine (context, state, url) =
      let emoji = case state of
            StatusState.ERROR    -> "❗"
            StatusState.EXPECTED -> "💤"
            StatusState.FAILURE  -> "❌"
            StatusState.PENDING  -> "⏳"
            StatusState.SUCCESS  -> "✅"
          link = case url of
            Nothing -> context
            Just url' -> "[" <> context <> "](" <> URI.unURI url' <> ")"
      in link <> " | " <> emoji

-- | Summarize the given StatusStates as a single StatusState.
summarizeStatuses :: [StatusState] -> StatusState
summarizeStatuses states
  | not (null states) && all (== StatusState.SUCCESS) states = StatusState.SUCCESS
  | StatusState.ERROR `elem` states = StatusState.ERROR
  | StatusState.FAILURE `elem` states = StatusState.FAILURE
  | otherwise = StatusState.PENDING
