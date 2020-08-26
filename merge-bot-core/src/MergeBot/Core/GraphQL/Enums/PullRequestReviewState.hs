{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.Enums.PullRequestReviewState where

import Data.GraphQL.Bootstrap

mkEnum "PullRequestReviewState"
  [ "APPROVED"
  , "CHANGES_REQUESTED"
  , "COMMENTED"
  , "DISMISSED"
  , "PENDING"
  ]
