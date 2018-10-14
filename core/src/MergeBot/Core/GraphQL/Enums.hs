{-|
Module      :  MergeBot.Core.GraphQL.Enums
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines enums defined in the GraphQL schema.
-}

module MergeBot.Core.GraphQL.Enums where

import Data.GraphQL.Query

{- TODO: THIS FILE SHOULD BE GENERATED -}

data StatusState
  = StatusState_EXPECTED
  | StatusState_ERROR
  | StatusState_FAILURE
  | StatusState_PENDING
  | StatusState_SUCCESS
  deriving (Show,Eq,Enum)

instance GraphQLEnum StatusState where
  parseEnum _ t = case fromText t of
    "EXPECTED" -> StatusState_EXPECTED
    "ERROR" -> StatusState_ERROR
    "FAILURE" -> StatusState_FAILURE
    "PENDING" -> StatusState_PENDING
    "SUCCESS" -> StatusState_SUCCESS
    s -> error $ "Invalid StatusState: " ++ s
