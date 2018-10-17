{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GraphQL.Test.AllTypes where

import Data.Aeson (decodeFileStrict)
import qualified Data.Maybe as Maybe
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)

import Data.GraphQL

data Greeting = GreetingHELLO | GreetingGOODBYE
  deriving (Show)

instance GraphQLEnum Greeting where
  getEnum _ t = case fromText t of
    "HELLO" -> GreetingHELLO
    "GOODBYE" -> GreetingGOODBYE
    s -> error $ "Invalid Greeting: " ++ s

newtype Result = Result Value

schema :: Schema
schema = SchemaObject
  [ ("bool", SchemaBool)
  , ("int", SchemaInt)
  , ("double", SchemaDouble)
  , ("text", SchemaText)
  , ("scalar", SchemaScalar)
  , ("enum", SchemaEnum (Proxy :: Proxy Greeting))
  , ("maybeNull", SchemaMaybe $ SchemaObject [("text", SchemaText)])
  , ("maybeObject", SchemaMaybe $ SchemaObject [("text", SchemaText)])
  , ("list", SchemaList $ SchemaObject
      [ ("type", SchemaText)
      , ("maybeBool", SchemaMaybe SchemaBool)
      , ("maybeInt", SchemaMaybe SchemaInt)
      , ("maybeDouble", SchemaMaybe SchemaDouble)
      ]
    )
  ]

get :: QuasiQuoter
get = getterFor 'Result schema

result :: Result
result = $(do
  obj <- runIO $ decodeFileStrict "test/testdata/all_types.json"
  [| Result $(lift $ Maybe.fromJust (obj :: Maybe Value)) |]
  )
