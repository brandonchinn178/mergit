{-|
Module      :  Data.GraphQL.Result.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helpers for parsing GraphQL results.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GraphQL.Result.Parse
  ( getBool
  , getInt
  , getDouble
  , getText
  , getScalar
  , getMaybe
  , getList
  , getObject
  , atKey
  , GraphQLEnum(..)
  , fromText
  -- * Re-exports
  , Proxy(..)
  ) where

import Data.Aeson (Object, Value(..))
import Data.HashMap.Strict ((!))
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Data.Text as Text

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
getDouble :: Value -> Int
getDouble = \case
  Number n | Right d <- floatingOrInteger' n -> d
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
getMaybe :: (Value -> a) -> Value -> Maybe a
getMaybe f = \case
  Null -> Nothing
  v -> Just $ f v

-- | Parse a List.
getList :: (Value -> a) -> Value -> [a]
getList f = \case
  Array xs -> map f $ Vector.toList xs
  v -> error $ "Unexpected value (expected List): " ++ show v

-- | Parse an Object.
getObject :: Value -> Object
getObject = \case
  Object o -> o
  v -> error $ "Unexpected value (expected Object): " ++ show v

-- | Use the given parser at the given key.
atKey :: Text -> (Value -> a) -> Object -> a
atKey key f o = f $ o ! key

-- | An alias for Text.unpack.
fromText :: Text -> String
fromText = Text.unpack

-- | A type class for parsing SchemaEnum.
class GraphQLEnum e where
  getEnum :: Proxy e -> Text -> e
