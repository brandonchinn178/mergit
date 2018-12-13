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
  , GraphQLScalar(..)
  , ToScalar
  , parseValueScalar
  ) where

import Data.Aeson (Value(String))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)

class GraphQLEnum e where
  getEnum :: Text -> e

type family ToEnum (s :: Symbol) :: Type

parseValueEnum :: forall e. GraphQLEnum e => Maybe Value -> Maybe e
parseValueEnum = \case
  Just (String t) -> Just $ getEnum @e t
  _ -> Nothing

class GraphQLScalar e where
  getScalar :: Value -> e

type family ToScalar (s :: Symbol) :: Type

parseValueScalar :: forall e. GraphQLScalar e => Maybe Value -> Maybe e
parseValueScalar = fmap (getScalar @e)
