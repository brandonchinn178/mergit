{-# LANGUAGE TemplateHaskell #-}

module Mergit.Core.GraphQL.Enums.StatusState where

import Data.GraphQL.Bootstrap

mkEnum "StatusState"
  [ "ERROR"
  , "EXPECTED"
  , "FAILURE"
  , "PENDING"
  , "SUCCESS"
  ]
