{-|
Module      :  MergeBot.Core.GraphQL.Branches
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Branches graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Branches where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

import MergeBot.Core.GraphQL.Enums

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  } deriving (Show)

newtype Result = Result Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    ]

instance IsQueryable IO Result where
  execQuery = execQueryIO

query :: Query Result
query = $(readGraphQLFile "Branches.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'Result schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "refs"
        , SchemaMaybe $ SchemaObject
          [ ( "edges"
            , SchemaMaybe $ SchemaList $ SchemaMaybe $ SchemaObject
              [ ("cursor", SchemaText)
              , ( "node"
                , SchemaMaybe $ SchemaObject
                  [ ("name", SchemaText)
                  , ( "target"
                    , SchemaObject
                      [ ( "status"
                        , SchemaMaybe $ SchemaObject
                          [ ( "contexts"
                            , SchemaList $ SchemaObject
                              [ ("context", SchemaText)
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
