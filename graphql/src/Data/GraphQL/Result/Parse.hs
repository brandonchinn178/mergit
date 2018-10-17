{-|
Module      :  Data.GraphQL.Result.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helpers for parsing GraphQL results.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.GraphQL.Result.Parse
  ( getBool
  , getInt
  , getDouble
  , getText
  , getScalar
  , mapMaybe
  , mapList
  , getObject
  , getKey
  , lookupKey
  , fromJust
  , GraphQLEnum(..)
  , fromText
  -- * Re-exports
  , Proxy(..)
  ) where

import Data.Aeson (Object, Value(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Stack (HasCallStack)

-- | Parse a Bool.
getBool :: Value -> Bool
getBool = \case
  Bool b -> b
  v -> error $ "Unexpected value (expected Bool): " ++ show v

-- | Helper for parsing Number.
floatingOrInteger' :: Scientific -> Either Double Int
floatingOrInteger' = floatingOrInteger

-- | Parse an Int.
getInt :: Value -> Int
getInt = \case
  Number n | Right i <- floatingOrInteger' n -> i
  v -> error $ "Unexpected value (expected Int): " ++ show v

-- | Parse a Double.
getDouble :: Value -> Double
getDouble = \case
  Number n | Left d <- floatingOrInteger' n -> d
  v -> error $ "Unexpected value (expected Double): " ++ show v

-- | Parse a Text.
getText :: Value -> Text
getText = \case
  String t -> t
  v -> error $ "Unexpected value (expected Text): " ++ show v

-- | Parse a Scalar.
getScalar :: Value -> Text
getScalar = \case
  String t -> t
  v -> error $ "Unexpected value (expected Scalar): " ++ show v

-- | Parse a Maybe value.
getMaybe :: Value -> Maybe Value
getMaybe = \case
  Null -> Nothing
  v -> Just v

-- | Parse a Maybe value, mapping over the wrapped element with the given parser.
mapMaybe :: (Value -> a) -> Value -> Maybe a
mapMaybe f = fmap f . getMaybe

-- | Parse a List, mapping over the elements with the given parser.
mapList :: (Value -> a) -> Value -> [a]
mapList f = \case
  Array xs -> map f $ Vector.toList xs
  v -> error $ "Unexpected value (expected List): " ++ show v

-- | Parse an Object.
getObject :: Value -> Object
getObject = \case
  Object o -> o
  v -> error $ "Unexpected value (expected Object): " ++ show v

-- | Get the given key from the given Value.
getKey :: String -> Value -> Value
getKey key = fromJust . lookupKey key

-- | Lookup the given key from the given Value.
lookupKey :: String -> Value -> Value
lookupKey key = Maybe.fromMaybe Null . HashMap.lookup (Text.pack key) . getObject

-- | Data.Maybe.fromJust for JSON Values.
fromJust :: HasCallStack => Value -> Value
fromJust = Maybe.fromJust . getMaybe

-- | An alias for Text.unpack.
fromText :: Text -> String
fromText = Text.unpack

-- | A type class for parsing SchemaEnum.
class GraphQLEnum e where
  getEnum :: Proxy e -> Text -> e
