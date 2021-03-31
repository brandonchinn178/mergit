{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.Enums.MergeableState where

import Data.GraphQL.Bootstrap

mkEnum "MergeableState"
  [ "CONFLICTING"
  , "MERGEABLE"
  , "UNKNOWN"
  ]
