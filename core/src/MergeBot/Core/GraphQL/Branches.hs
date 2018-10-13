{-|
Module      :  MergeBot.Core.GraphQL.Branches
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Branches graphql query.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Branches where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL.Query

import MergeBot.Core.GraphQL.Enums

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  } deriving (Show)

newtype Result = Result Object
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs _ args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    ]

query :: Query Result
query = $(readGraphQLFile "Branches.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "refs"
        , SchemaMaybe $ SchemaObject
          [ ( "edges"
            , SchemaMaybe $ SchemaList $ SchemaMaybe $ SchemaObject
              [ ("cursor", SchemaString)
              , ( "node"
                , SchemaMaybe $ SchemaObject
                  [ ("name", SchemaString)
                  , ( "target"
                    , SchemaObject
                      [ ( "status"
                        , SchemaMaybe $ SchemaObject
                          [ ( "contexts"
                            , SchemaList $ SchemaObject
                              [ ("context", SchemaString)
                              , ("state", SchemaEnum (Proxy :: Proxy StatusState))
                              ]
                            )
                          ]
                        )
                      ]
                    )
                  ]
                )
              ]
            )
          ]
        )
      ]
    )
  ]
