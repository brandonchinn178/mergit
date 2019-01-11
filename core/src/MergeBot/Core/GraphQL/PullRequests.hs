{-|
Module      :  MergeBot.Core.GraphQL.PullRequests
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequests graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequests where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL
import Data.GraphQL.Aeson

import MergeBot.Core.GraphQL.API (API)

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  } deriving (Show)

data Result

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    ]

query :: Query API Schema
query = $(readGraphQLFile "PullRequests.graphql") -- TODO: when generated, will actually be file contents

type Schema = 'SchemaObject
  '[ '( "repository", 'SchemaObject
        '[ '( "pullRequests", 'SchemaObject
              '[ '( "pageInfo", 'SchemaObject
                    '[ '("hasNextPage", 'SchemaBool)
                     , '("endCursor", 'SchemaMaybe 'SchemaText)
                     ]
                  )
               , '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                    '[ '( "number", 'SchemaInt )
                     , '( "title", 'SchemaText )
                     , '( "author", 'SchemaMaybe ('SchemaObject
                          '[ '("login", 'SchemaText)
                           ]
                        ))
                     , '( "createdAt", 'SchemaScalar "DateTime" )
                     , '( "updatedAt", 'SchemaScalar "DateTime" )
                     ]
                  ))))
               ]
            )
         ]
      )
   ]
