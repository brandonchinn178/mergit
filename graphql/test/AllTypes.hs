{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module AllTypes where

import Data.Aeson (Value(..))
import qualified Data.Text as Text

import Data.GraphQL
import Data.GraphQL.Schema.Internal (Object(..))
import Util (getMockedResult)

{- Greeting enum -}

data Greeting = HELLO | GOODBYE
  deriving (Show)

instance GraphQLEnum Greeting where
  getEnum s = case Text.unpack s of
    "HELLO" -> HELLO
    "GOODBYE" -> GOODBYE
    _ -> error $ "Bad Greeting: " ++ Text.unpack s

type instance ToEnum "Greeting" = Greeting

instance FromSchema Greeting where
  type ToSchema Greeting = 'SchemaEnum "Greeting"
  parseValue = parseValueEnum

{- Coordinate scalar -}

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show)

instance GraphQLScalar Coordinate where
  getScalar = \case
    String s -> case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> Coordinate (x, y)
      _ -> error $ "Bad Coordinate: " ++ Text.unpack s
    v -> error $ "Invalid Coordinate: " ++ show v

type instance ToScalar "Coordinate" = Coordinate

instance FromSchema Coordinate where
  type ToSchema Coordinate = 'SchemaScalar "Coordinate"
  parseValue = parseValueScalar

{- AllTypes result -}

type Schema = 'SchemaObject
  '[ '("bool", 'SchemaBool)
   , '("int", 'SchemaInt)
   , '("int2", 'SchemaInt)
   , '("double", 'SchemaDouble)
   , '("text", 'SchemaText)
   , '("scalar", 'SchemaScalar "Coordinate")
   , '("enum", 'SchemaEnum "Greeting")
   , '("maybeObject", 'SchemaMaybe ('SchemaObject
        '[ '("text", 'SchemaText)
         ]
      ))
   , '("maybeObjectNull", 'SchemaMaybe ('SchemaObject
        '[ '("text", 'SchemaText)
         ]
      ))
   , '("maybeList", 'SchemaMaybe ('SchemaList ('SchemaObject
        '[ '("text", 'SchemaText)
         ]
      )))
   , '("maybeListNull", 'SchemaMaybe ('SchemaList ('SchemaObject
        '[ '("text", 'SchemaText)
         ]
      )))
   , '("list", 'SchemaList ('SchemaObject
        '[ '("type", 'SchemaText)
         , '("maybeBool", 'SchemaMaybe 'SchemaBool)
         , '("maybeInt", 'SchemaMaybe 'SchemaInt)
         , '("maybeNull", 'SchemaMaybe 'SchemaBool)
         ]
      ))
   , '("nonexistent", 'SchemaMaybe 'SchemaText)
   ]

result :: Object Schema
result = $(getMockedResult "test/all_types.json")
