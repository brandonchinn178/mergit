{-# LANGUAGE TemplateHaskell #-}

module Mergit.Core.GraphQL.Enums.CheckStatusState where

import Data.GraphQL.Bootstrap

mkEnum "CheckStatusState"
  [ "COMPLETED"
  , "IN_PROGRESS"
  , "QUEUED"
  , "REQUESTED"
  , "WAITING"
  ]
