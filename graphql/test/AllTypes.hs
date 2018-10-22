{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AllTypes where

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

newtype Result = UnsafeResult Value

schema :: Schema
schema = SchemaObject
  [ ("bool", SchemaBool)
  , ("int", SchemaInt)
  , ("double", SchemaDouble)
  , ("text", SchemaText)
  , ("scalar", SchemaScalar)
  , ("enum", SchemaEnum (Proxy :: Proxy Greeting))
  , ("maybeObject", SchemaMaybe $ SchemaObject [("text", SchemaText)])
  , ("maybeObjectNull", SchemaMaybe $ SchemaObject [("text", SchemaText)])
  , ("maybeList", SchemaMaybe $ SchemaList $ SchemaObject [("text", SchemaText)])
  , ("maybeListNull", SchemaMaybe $ SchemaList $ SchemaObject [("text", SchemaText)])
  , ("list", SchemaList $ SchemaObject
      [ ("type", SchemaText)
      , ("maybeBool", SchemaMaybe SchemaBool)
      , ("maybeInt", SchemaMaybe SchemaInt)
      , ("maybeNull", SchemaMaybe SchemaBool)
      ]
    )
  , ("nonexistent", SchemaMaybe SchemaText)
  ]

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

result :: Result
result = $(do
  obj <- runIO $ decodeFileStrict "test/all_types.json"
  [| UnsafeResult $(lift $ Maybe.fromJust (obj :: Maybe Value)) |]
  )
