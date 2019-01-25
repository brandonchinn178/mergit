{-|
Module      :  MergeBot.Core.GraphQL.PullRequestReview
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequestReview graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequestReview where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _number    :: Int
  , _after     :: Maybe String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "number"    .= _number args
    , "after"     .= _after args
    ]

query :: Query
query = $(readGraphQLFile "PullRequestReview.graphql") -- TODO: when generated, will actually be file contents

type Schema = 'SchemaObject
  '[ '( "repository", 'SchemaObject
        '[ '( "pullRequest", 'SchemaMaybe ('SchemaObject
              '[ '( "reviews", 'SchemaMaybe ('SchemaObject
                    '[ '( "pageInfo", 'SchemaObject
                          '[ '("hasNextPage", 'SchemaBool)
                           , '("endCursor", 'SchemaMaybe 'SchemaText)
                           ]
                        )
                     , '( "nodes", 'SchemaMaybe ('SchemaList ('SchemaMaybe ('SchemaObject
                          '[ '( "author", 'SchemaMaybe ('SchemaObject
                                '[ '("login", 'SchemaText)
                                 ]
                              ))
                           , '( "state", 'SchemaEnum "PullRequestReviewState" )
                           ]
                        ))))
                     ]
                  ))
               ]
            ))
         ]
      )
   ]
