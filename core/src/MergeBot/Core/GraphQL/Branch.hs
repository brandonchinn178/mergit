{-|
Module      :  MergeBot.Core.GraphQL.Branch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Branch graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Branch where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL
import Data.GraphQL.Aeson

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _name      :: String
  } deriving (Show)

data Result

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "name"      .= _name args
    ]

query :: Query Schema
query = $(readGraphQLFile "Branch.graphql") -- TODO: when generated, will actually be file contents

type Schema = 'SchemaObject
  '[ '( "repository", 'SchemaObject
        '[ '( "ref", 'SchemaMaybe ('SchemaObject
              '[ '( "target", 'SchemaObject
                    '[ '( "oid", 'SchemaScalar "GitObjectID" )
                     , '( "message", 'SchemaMaybe 'SchemaText )
                     , '( "tree", 'SchemaMaybe ('SchemaObject
                          '[ '( "oid", 'SchemaScalar "GitObjectID" )
                           , '( "entries", 'SchemaMaybe ('SchemaList ('SchemaObject
                                '[ '( "name", 'SchemaText )
                                 , '( "object", 'SchemaMaybe ('SchemaObject
                                     '[ '("text", 'SchemaMaybe 'SchemaText)
                                      ]
                                    ))
                                 ]
                             )))
                           ]
                        ))
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
            ))
         ]
      )
   ]
