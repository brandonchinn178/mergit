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

import Data.GraphQL
import Data.GraphQL.Aeson

import MergeBot.Core.GraphQL.API (API)

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _number    :: Int
  , _after     :: Maybe String
  } deriving (Show)

data Result

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "number"    .= _number args
    , "after"     .= _after args
    ]

query :: Query API Schema
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
