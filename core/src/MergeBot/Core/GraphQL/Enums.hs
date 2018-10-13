{-|
Module      :  MergeBot.Core.GraphQL.Enums
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines enums defined in the GraphQL schema.
-}

module MergeBot.Core.GraphQL.Enums where

{- TODO: THIS FILE SHOULD BE GENERATED -}

data StatusState
  = StatusState_EXPECTED
  | StatusState_ERROR
  | StatusState_FAILURE
  | StatusState_PENDING
  | StatusState_SUCCESS
  deriving (Show,Eq,Enum)