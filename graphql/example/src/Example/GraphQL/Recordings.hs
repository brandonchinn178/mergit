{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.GraphQL.Recordings where

import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))

import Example.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _query :: String
  , _first :: Maybe Int
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "query" .= _query args
    , "first" .= _first args
    ]

query :: Query
query = $(readGraphQLFile "Recordings.graphql")

type Schema = 'SchemaObject
  '[ '("search", 'SchemaMaybe ('SchemaObject
        '[ '( "recordings", 'SchemaMaybe ('SchemaObject
              '[ '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                    '[ '( "title", 'SchemaMaybe 'SchemaText )
                     , '( "artists", 'SchemaMaybe ('SchemaObject
                          '[ '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                                '[ '("name", 'SchemaMaybe 'SchemaText)
                                 ]
                              ))))
                           ]
                        ))
                     , '( "video", 'SchemaMaybe 'SchemaBool )
                     , '( "length", 'SchemaMaybe ('SchemaScalar "Duration") )
                     , '( "rating", 'SchemaMaybe ('SchemaObject
                          '[ '("voteCount", 'SchemaInt)
                           , '("value", 'SchemaMaybe 'SchemaDouble)
                           ]
                        ))
                     , '( "releases", 'SchemaMaybe ('SchemaObject
                          '[ '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                                '[ '( "title", 'SchemaMaybe 'SchemaText )
                                 , '( "date", 'SchemaMaybe ('SchemaScalar "Date") )
                                 , '( "status", 'SchemaMaybe ('SchemaEnum "ReleaseStatus") )
                                 ]
                              ))))
                           ]
                        ))
                     ]
                  ))))
               ]
            ))
         ]
      ))
   ]
