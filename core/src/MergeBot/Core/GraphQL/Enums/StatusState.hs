{-|
Module      :  MergeBot.Core.GraphQL.Enums.StatusState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the StatusState enum.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Enums.StatusState where

import Data.GraphQL
import qualified Data.Text as Text

{- TODO: THIS FILE SHOULD BE GENERATED -}

data StatusState
  = EXPECTED
  | ERROR
  | FAILURE
  | PENDING
  | SUCCESS
  deriving (Show,Eq)

instance GraphQLEnum StatusState where
  getEnum s = case Text.unpack s of
    "EXPECTED" -> EXPECTED
    "ERROR" -> ERROR
    "FAILURE" -> FAILURE
    "PENDING" -> PENDING
    "SUCCESS" -> SUCCESS
    _ -> error $ "Bad StatusState: " ++ Text.unpack s

type instance ToEnum "StatusState" = StatusState

instance FromSchema StatusState where
  type ToSchema StatusState = 'SchemaEnum "StatusState"
  parseValue = parseValueEnum
