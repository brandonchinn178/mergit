{-|
Module      :  MergeBot.Core.GraphQL.PullRequests
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PullRequests graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.PullRequests where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

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
query = $(readGraphQLFile "PullRequests.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'Result schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "pullRequests"
        , SchemaObject
          [ ( "edges"
            , SchemaMaybe $ SchemaList $ SchemaMaybe $ SchemaObject
              [ ("cursor", SchemaText)
              , ( "node"
                , SchemaMaybe $ SchemaObject
                  [ ("number", SchemaInt)
                  , ("title", SchemaText)
                  , ( "author"
                    , SchemaMaybe $ SchemaObject
                      [ ("login", SchemaText)
                      ]
                    )
                  , ("createdAt", SchemaScalar)
                  , ("updatedAt", SchemaScalar)
                  ]
                )
              ]
            )
          ]
        )
      ]
    )
  ]
