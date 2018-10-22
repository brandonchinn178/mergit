{-|
Module      :  MergeBot.Core.GraphQL.PullRequestReviewState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequestReviewState enum.
-}

module MergeBot.Core.GraphQL.PullRequestReviewState where

import Data.GraphQL

{- TODO: THIS FILE SHOULD BE GENERATED -}

data PullRequestReviewState
  = PENDING
  | COMMENTED
  | APPROVED
  | CHANGES_REQUESTED
  | DISMISSED
  deriving (Show,Eq,Enum)

instance GraphQLEnum PullRequestReviewState where
  getEnum _ t = case fromText t of
    "PENDING" -> PENDING
    "COMMENTED" -> COMMENTED
    "APPROVED" -> APPROVED
    "CHANGES_REQUESTED" -> CHANGES_REQUESTED
    "DISMISSED" -> DISMISSED
    s -> error $ "Invalid PullRequestReviewState: " ++ s
