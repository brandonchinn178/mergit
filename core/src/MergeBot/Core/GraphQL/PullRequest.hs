{-|
Module      :  MergeBot.Core.GraphQL.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequest graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequest where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _number    :: Int
  } deriving (Show)

newtype Result = UnsafeResult Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "number"    .= _number args
    ]

instance IsQueryable Result where
  execQuery = execQueryFor UnsafeResult

query :: Query Result
query = $(readGraphQLFile "PullRequest.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "pullRequest"
        , SchemaMaybe $ SchemaObject
          [ ("number", SchemaInt)
          , ("title", SchemaText)
          , ( "author"
            , SchemaMaybe $ SchemaObject
              [ ("login", SchemaText)
              ]
            )
          , ("createdAt", SchemaScalar)
          , ("updatedAt", SchemaScalar)
          , ("url", SchemaScalar)
          , ("bodyHTML", SchemaScalar)
          , ("headRefOid", SchemaScalar)
          , ("headRefName", SchemaText)
          , ("baseRefName", SchemaText)
          ]
        )
      ]
    )
  ]
