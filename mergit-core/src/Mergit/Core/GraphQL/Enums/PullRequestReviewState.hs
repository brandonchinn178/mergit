{-# LANGUAGE TemplateHaskell #-}

module Mergit.Core.GraphQL.Enums.PullRequestReviewState where

import Data.GraphQL.Bootstrap

mkEnum "PullRequestReviewState"
  [ "APPROVED"
  , "CHANGES_REQUESTED"
  , "COMMENTED"
  , "DISMISSED"
  , "PENDING"
  ]
