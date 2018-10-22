{-|
Module      :  MergeBot.Core.GraphQL.StatusState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the StatusState enum.
-}

module MergeBot.Core.GraphQL.StatusState where

import Data.GraphQL

{- TODO: THIS FILE SHOULD BE GENERATED -}

data StatusState
  = EXPECTED
  | ERROR
  | FAILURE
  | PENDING
  | SUCCESS
  deriving (Show,Eq,Enum)

instance GraphQLEnum StatusState where
  getEnum _ t = case fromText t of
    "EXPECTED" -> EXPECTED
    "ERROR" -> ERROR
    "FAILURE" -> FAILURE
    "PENDING" -> PENDING
    "SUCCESS" -> SUCCESS
    s -> error $ "Invalid StatusState: " ++ s
