{-|
Module      :  Data.GraphQL.Schema.Custom
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Type classes for converting custom GraphQL types.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Schema.Custom
  ( GraphQLEnum(..)
  , ToEnum
  , parseValueEnum
  , GraphQLScalar
  , ToScalar
  , parseValueScalar
  ) where

import Data.Aeson (FromJSON(..), Value(String))
import Data.Aeson.Types (parseMaybe)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)

class Enum e => GraphQLEnum e where
  getEnum :: Text -> e

type family ToEnum (s :: Symbol) :: Type

parseValueEnum :: forall e. GraphQLEnum e => Maybe Value -> Maybe e
parseValueEnum = \case
  Just (String t) -> Just $ getEnum @e t
  _ -> Nothing

class FromJSON s => GraphQLScalar s where

type family ToScalar (s :: Symbol) :: Type

parseValueScalar :: FromJSON e => Maybe Value -> Maybe e
parseValueScalar = (>>= parseMaybe parseJSON)
