{-|
Module      :  MergeBot.Core.GraphQL.PullRequestReview
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequestReview graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequestReview where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

import MergeBot.Core.GraphQL.PullRequestReviewState (PullRequestReviewState)

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _number    :: Int
  , _after     :: Maybe String
  } deriving (Show)

newtype Result = UnsafeResult Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "number"    .= _number args
    , "after"     .= _after args
    ]

instance IsQueryable Result where
  execQuery = execQueryFor UnsafeResult

query :: Query Result
query = $(readGraphQLFile "PullRequestReview.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "pullRequest"
        , SchemaMaybe $ SchemaObject
          [ ( "reviews"
            , SchemaMaybe $ SchemaObject
              [ ( "pageInfo"
                , SchemaObject
                  [ ("hasNextPage", SchemaBool)
                  , ("endCursor", SchemaMaybe SchemaText)
                  ]
                )
              , ( "nodes"
                , SchemaMaybe $ SchemaList $ SchemaMaybe $ SchemaObject
                  [ ( "author"
                    , SchemaMaybe $ SchemaObject
                      [ ("login", SchemaText)
                      ]
                    )
                  , ("state", SchemaEnum (Proxy :: Proxy PullRequestReviewState))
                  ]
                )
              ]
            )
          ]
        )
      ]
    )
  ]
