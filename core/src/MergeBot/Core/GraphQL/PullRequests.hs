{-|
Module      :  MergeBot.Core.GraphQL.PullRequests
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequests graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequests where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  } deriving (Show)

newtype Result = UnsafeResult Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    ]

instance IsQueryable Result where
  execQuery = execQueryFor UnsafeResult

query :: Query Result
query = $(readGraphQLFile "PullRequests.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "pullRequests"
        , SchemaObject
          [ ( "pageInfo"
            , SchemaObject
              [ ("hasNextPage", SchemaBool)
              , ("endCursor", SchemaMaybe SchemaText)
              ]
            )
          , ( "nodes"
            , SchemaMaybe $ SchemaList $ SchemaMaybe $ SchemaObject
                [ ("number", SchemaInt)
                , ("title", SchemaText)
                , ( "author"
                  , SchemaMaybe $ SchemaObject
                    [ ("login", SchemaText)
                    ]
                  )
                , ("createdAt", SchemaScalar)
                , ("updatedAt", SchemaScalar)
                ]
            )
          ]
        )
      ]
    )
  ]
