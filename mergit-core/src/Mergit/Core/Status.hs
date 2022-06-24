{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      :  Mergit.Core.Status
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for resolving CI statuses.
-}
module Mergit.Core.Status (
  getCIStatus,
  resolveCIStatus,
  displayCIStatus,
) where

import Data.Aeson.Schema (get, unwrap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Printf (printf)

import Mergit.Core.Config (MergitConfig (..))
import Mergit.Core.GitHub (CIContext)
import Mergit.Core.GraphQL.Enums.StatusState (StatusState)
import Mergit.Core.GraphQL.Enums.StatusState qualified as StatusState

type CIContextInfo = [unwrap| CIContext.(context, state, targetUrl) |]

data CIStatus = CIStatus
  { ciContexts :: [CIContextInfo]
  -- ^ Only contains contexts in the Mergit config, in the same order as in the config
  , ciErrorContexts :: [CIContextInfo]
  -- ^ If the CI didn't even start; e.g. submitting an invalid configuration file to Circle CI
  }
  deriving (Show)

{- | Get CI statuses compiled from the Mergit config and the contexts for a commit.

 Returns a map from context name to the state of the context and the associated URL.
-}
getCIStatus :: MergitConfig -> [CIContext] -> CIStatus
getCIStatus MergitConfig{requiredStatuses} contexts =
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
      , "ci/circleci_enterprise: Build Error"
      ]

-- | Resolve the given CIStatus as a single overall StatusState.
resolveCIStatus :: CIStatus -> StatusState
resolveCIStatus CIStatus{..} =
  if null ciErrorContexts
    then summarizeStatuses $ map (\(_, state, _) -> state) ciContexts
    else StatusState.ERROR

-- | Get text containing Markdown to display the given CIStatus.
displayCIStatus :: CIStatus -> Text
displayCIStatus CIStatus{..} =
  Text.pack . unlines $
    [ "CI Job | Status"
    , ":-----:|:-----:"
    ]
      ++ map mkLine (ciContexts ++ ciErrorContexts)
  where
    mkLine (context, state, url) =
      let emoji = case state of
            StatusState.ERROR -> "❗"
            StatusState.EXPECTED -> "💤"
            StatusState.FAILURE -> "❌"
            StatusState.PENDING -> "⏳"
            StatusState.SUCCESS -> "✅"
          link = maybe (Text.unpack context) (printf "[%s](%s)" context) url
       in printf "%s | %s" link (emoji :: Text)

-- | Summarize the given StatusStates as a single StatusState.
summarizeStatuses :: [StatusState] -> StatusState
summarizeStatuses states
  | not (null states) && all (== StatusState.SUCCESS) states = StatusState.SUCCESS
  | StatusState.ERROR `elem` states = StatusState.ERROR
  | StatusState.FAILURE `elem` states = StatusState.FAILURE
  | otherwise = StatusState.PENDING
