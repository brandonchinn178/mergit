{-|
Module      :  MergeBot.Core.GraphQL.Enums
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines enums defined in the GraphQL schema.
-}

module MergeBot.Core.GraphQL.Enums where

import Data.GraphQL

{- TODO: THIS FILE SHOULD BE GENERATED -}

data StatusState
  = StatusStateEXPECTED
  | StatusStateERROR
  | StatusStateFAILURE
  | StatusStatePENDING
  | StatusStateSUCCESS
  deriving (Show,Eq,Enum)

instance GraphQLEnum StatusState where
  getEnum _ t = case fromText t of
    "EXPECTED" -> StatusStateEXPECTED
    "ERROR" -> StatusStateERROR
    "FAILURE" -> StatusStateFAILURE
    "PENDING" -> StatusStatePENDING
    "SUCCESS" -> StatusStateSUCCESS
    s -> error $ "Invalid StatusState: " ++ s
