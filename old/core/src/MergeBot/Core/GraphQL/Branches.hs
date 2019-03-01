{-|
Module      :  MergeBot.Core.GraphQL.Branches
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Branches graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Branches where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    ]

query :: Query
query = $(readGraphQLFile "Branches.graphql") -- TODO: when generated, will actually be file contents

type Schema = 'SchemaObject
  '[ '( "repository", 'SchemaObject
        '[ '( "refs", 'SchemaMaybe ('SchemaObject
              '[ '( "pageInfo", 'SchemaObject
                    '[ '("hasNextPage", 'SchemaBool)
                     , '("endCursor", 'SchemaMaybe 'SchemaText)
                     ]
                  )
               , '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                    '[ '( "name", 'SchemaText )
                     , '( "target", 'SchemaObject
                          '[ '( "message", 'SchemaMaybe 'SchemaText )
                           , '( "status", 'SchemaMaybe ('SchemaObject
                                '[ '( "contexts", 'SchemaList ('SchemaObject
                                      '[ '("context", 'SchemaText)
                                       , '("state", 'SchemaEnum "StatusState")
                                       ]
                                    ))
                                 ]
                             ))
                           ]
                       )
                     ]
                  ))))
               ]
            ))
         ]
      )
   ]
