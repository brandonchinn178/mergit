{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Data.PullRequestReviewState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequestReviewState enum.
-}
module GitHub.Data.PullRequestReviewState where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum
  "PullRequestReviewState"
  [ "PENDING"
  , "COMMENTED"
  , "APPROVED"
  , "CHANGES_REQUESTED"
  , "DISMISSED"
  ]
