{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Recordings where

import Data.GraphQL
import Data.GraphQL.Aeson (object, (.=))

data Args = Args
  { _query :: String
  , _first :: Maybe Int
  } deriving (Show)

data Result

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs Args{..} = object
    [ "query" .= _query
    , "first" .= _first
    ]

query :: Query Schema
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
