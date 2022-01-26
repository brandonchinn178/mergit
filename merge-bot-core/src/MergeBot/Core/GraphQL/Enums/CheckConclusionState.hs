{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.Enums.CheckConclusionState where

import Data.GraphQL.Bootstrap

mkEnum "CheckConclusionState"
  [ "ACTION_REQUIRED"
  , "CANCELLED"
  , "FAILURE"
  , "NEUTRAL"
  , "SKIPPED"
  , "STALE"
  , "STARTUP_FAILURE"
  , "SUCCESS"
  , "TIMED_OUT"
  ]
