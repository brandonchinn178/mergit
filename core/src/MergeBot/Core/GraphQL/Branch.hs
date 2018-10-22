{-|
Module      :  MergeBot.Core.GraphQL.Branch
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Branch graphql query.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Branch where

{- TODO: THIS FILE SHOULD BE GENERATED -}

import Data.GraphQL

import MergeBot.Core.GraphQL.StatusState (StatusState)

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _name      :: String
  } deriving (Show)

newtype Result = UnsafeResult Value
  deriving (Show)

instance HasArgs Result where
  type QueryArgs Result = Args
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "name"      .= _name args
    ]

instance IsQueryable Result where
  execQuery = execQueryFor UnsafeResult

query :: Query Result
query = $(readGraphQLFile "Branch.graphql") -- TODO: when generated, will actually be file contents

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

schema :: Schema
schema = SchemaObject
  [ ( "repository"
    , SchemaObject
      [ ( "ref"
        , SchemaMaybe $ SchemaObject
          [ ( "target"
            , SchemaObject
              [ ( "status"
                , SchemaMaybe $ SchemaObject
                  [ ( "contexts"
                    , SchemaList $ SchemaObject
                      [ ("context", SchemaText)
                      , ("state", SchemaEnum (Proxy :: Proxy StatusState))
                      ]
                    )
                  ]
                )
              ]
            )
          ]
        )
      ]
    )
  ]
