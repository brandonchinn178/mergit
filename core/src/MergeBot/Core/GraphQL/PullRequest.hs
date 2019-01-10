{-|
Module      :  MergeBot.Core.GraphQL.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequest graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequest where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL
import Data.GraphQL.Aeson

import MergeBot.Core.GraphQL.API (API)

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _number    :: Int
  } deriving (Show)

data Result

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "number"    .= _number args
    ]

query :: Query API Schema
query = $(readGraphQLFile "PullRequest.graphql") -- TODO: when generated, will actually be file contents

type Schema = 'SchemaObject
  '[ '( "repository", 'SchemaObject
        '[ '( "pullRequest", 'SchemaMaybe ('SchemaObject
              '[ '( "number", 'SchemaInt )
               , '( "title", 'SchemaText )
               , '( "author", 'SchemaMaybe ('SchemaObject
                    '[ '("login", 'SchemaText)
                     ]
                  ))
               , '( "createdAt", 'SchemaScalar "DateTime" )
               , '( "updatedAt", 'SchemaScalar "DateTime" )
               , '( "url", 'SchemaScalar "URI" )
               , '( "bodyHTML", 'SchemaScalar "HTML" )
               , '( "headRefOid", 'SchemaScalar "GitObjectID" )
               , '( "headRefName", 'SchemaText )
               , '( "baseRefName", 'SchemaText )
               ]
            ))
         ]
      )
   ]
