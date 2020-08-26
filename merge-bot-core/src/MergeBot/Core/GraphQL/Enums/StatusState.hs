{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.Enums.StatusState where

import Data.GraphQL.Bootstrap

mkEnum "StatusState"
  [ "ERROR"
  , "EXPECTED"
  , "FAILURE"
  , "PENDING"
  , "SUCCESS"
  ]
