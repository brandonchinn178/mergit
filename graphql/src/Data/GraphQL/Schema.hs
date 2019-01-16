{-|
Module      :  Data.GraphQL.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL response objects and the schema encoded in it.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GraphQL.Schema
  ( Object
  , SchemaGraph(..)
  , SchemaType
  , FromSchema(..)
  , GraphQLEnum(..)
  , ToEnum
  , parseValueEnum
  , GraphQLScalar
  , ToScalar
  , parseValueScalar
  , getKey
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Data.Vector as Vector
import Fcf (type (<=<), type (=<<), Eval, Find, FromMaybe, Fst, Snd, TyEq)
import GHC.TypeLits
    (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import Data.GraphQL.Aeson (toDouble, toInt)
import Data.GraphQL.Schema.Custom
import Data.GraphQL.Schema.Internal

{- FromSchema parsing -}

-- | Constraints that @FromSchema result@ needs to satisfy.
type family FromSchemaConstraints (schema :: SchemaType) result :: Constraint where
  FromSchemaConstraints ('SchemaScalar _) result = GraphQLScalar result
  FromSchemaConstraints ('SchemaEnum _) result = GraphQLEnum result
  FromSchemaConstraints _ _ = ()

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class FromSchema result where
  type ToSchema result = (schema :: SchemaType) | schema -> result
  parseValue :: Maybe Aeson.Value -> Maybe result

instance FromSchema Bool where
  type ToSchema Bool = 'SchemaBool
  parseValue = \case
    Just (Aeson.Bool b) -> Just b
    _ -> Nothing

instance FromSchema Int where
  type ToSchema Int = 'SchemaInt
  parseValue = \case
    Just (Aeson.Number n) | Just i <- toInt n -> Just i
    _ -> Nothing

instance FromSchema Double where
  type ToSchema Double = 'SchemaDouble
  parseValue = \case
    Just (Aeson.Number n) | Just d <- toDouble n -> Just d
    _ -> Nothing

instance FromSchema Text.Text where
  type ToSchema Text.Text = 'SchemaText
  parseValue = \case
    Just (Aeson.String t) -> Just t
    _ -> Nothing

instance FromSchema inner => FromSchema (Maybe inner) where
  type ToSchema (Maybe inner) = ('SchemaMaybe (ToSchema inner))
  parseValue = \case
    Just Aeson.Null -> Just Nothing
    Nothing -> Just Nothing
    v -> Just <$> parseValue v

instance FromSchema inner => FromSchema [inner] where
  type ToSchema [inner] = ('SchemaList (ToSchema inner))
  parseValue = \case
    Just (Aeson.Array a) -> mapM (parseValue . Just) $ Vector.toList a
    _ -> Nothing

instance FromSchema (Object ('SchemaObject inner)) where
  type ToSchema (Object ('SchemaObject inner)) = 'SchemaObject inner
  parseValue = \case
    Just (Aeson.Object o) -> Just $ UnsafeObject o
    _ -> Nothing

{- SchemaObject lookup -}

-- | The type-level function that return the schema of the given key in a 'SchemaObject'.
type family LookupSchema (key :: Symbol) (schema :: SchemaType) :: SchemaType where
  LookupSchema key ('SchemaObject schema) = Eval
    ( Snd
    =<< FromMaybe (TypeError
      (     'Text "Key '"
      ':<>: 'Text key
      ':<>: 'Text "' does not exist in the following schema:"
      ':$$: 'ShowType schema
      ))
    =<< Find (TyEq key <=< Fst) schema
    )
  LookupSchema key schema = TypeError
    (     'Text "Attempted to lookup key '"
    ':<>: 'Text key
    ':<>: 'Text "' in the following schema:"
    ':$$: 'ShowType schema
    )

-- | Get a key from the given 'Object', returned as the type encoded in its schema.
--
-- > let o = .. :: Object
-- >             ( 'SchemaObject
-- >                '[ '("foo", 'SchemaInt)
-- >                 , '("bar", 'SchemaObject
-- >                      '[ '("name", 'SchemaText)
-- >                       ]
-- >                 , '("baz", 'SchemaMaybe 'SchemaBool)
-- >                 ]
-- >             )
-- >
-- > getKey @"foo" o                  :: Bool
-- > getKey @"bar" o                  :: Object ('SchemaObject '[ '("name", 'SchemaText) ])
-- > getKey @"name" $ getKey @"bar" o :: Text
-- > getKey @"baz" o                  :: Maybe Bool
--
getKey
  :: forall key schema endSchema result
   . ( endSchema ~ LookupSchema key schema    -- lookup key in schema for resulting schema
     , ToSchema result ~ endSchema            -- the final result type's associated schema should
                                              -- match resulting schema
     , FromSchema result                      -- ensure result can be converted from a JSON object
     , FromSchemaConstraints endSchema result -- add any additional constraints on the result type
     , KnownSymbol key
     , Typeable endSchema
     )
  => Object schema
  -> result
getKey (UnsafeObject object) = fromMaybe invalidParse $ parseValue @result value
  where
    key = symbolVal (Proxy @key)
    value = HashMap.lookup (Text.pack key) object
    endSchema = prettyShow @endSchema
    invalidParse = error $ concat $ case value of
      Just _ -> ["Key missing from Object: ", key]
      Nothing ->
        ["Could not cast `", show value, "` at key '", key, "' to match schema: ", endSchema]
