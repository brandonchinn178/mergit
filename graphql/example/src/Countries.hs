{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Countries where

import Data.GraphQL

data Args = Args

newtype Result = UnsafeResult Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs _ = object []

instance IsQueryable Result where
  execQuery = execQueryFor UnsafeResult

query :: Query Result
query = $(readGraphQLFile "Countries.graphql")

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

schema :: Schema
schema = SchemaObject
  [ ("countries", SchemaList $ SchemaObject
      [ ("name", SchemaText)
      , ("continent", SchemaObject
          [("code", SchemaText)]
        )
      ]
    )
  ]
