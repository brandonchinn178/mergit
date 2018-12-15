{-|
Module      :  MergeBot.Core.GraphQL.Enums.PullRequestReviewState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequestReviewState enum.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Enums.PullRequestReviewState where

import Data.GraphQL
import qualified Data.Text as Text

{- TODO: THIS FILE SHOULD BE GENERATED -}

data PullRequestReviewState
  = PENDING
  | COMMENTED
  | APPROVED
  | CHANGES_REQUESTED
  | DISMISSED
  deriving (Show,Eq)

instance GraphQLEnum PullRequestReviewState where
  getEnum s = case Text.unpack s of
    "PENDING" -> PENDING
    "COMMENTED" -> COMMENTED
    "APPROVED" -> APPROVED
    "CHANGES_REQUESTED" -> CHANGES_REQUESTED
    "DISMISSED" -> DISMISSED
    _ -> error $ "Bad PullRequestReviewState: " ++ Text.unpack s

type instance ToEnum "PullRequestReviewState" = PullRequestReviewState

instance FromSchema PullRequestReviewState where
  type ToSchema PullRequestReviewState = 'SchemaEnum "PullRequestReviewState"
  parseValue = parseValueEnum
