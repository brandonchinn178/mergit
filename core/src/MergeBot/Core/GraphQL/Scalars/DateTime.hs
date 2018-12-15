{-|
Module      :  MergeBot.Core.GraphQL.Scalars.DateTime
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the DateTime scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.DateTime where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..))
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)

newtype DateTime = DateTime { unDateTime :: UTCTime }
  deriving (Show)

instance GraphQLScalar DateTime where
  getScalar = \case
    String s ->
      let time = Text.unpack s
          invalid = error $ "Invalid DateTime: " ++ time
      in maybe invalid DateTime $ parseISO8601 time
    v -> error $ "Invalid DateTime: " ++ show v

type instance ToScalar "DateTime" = DateTime

instance FromSchema DateTime where
  type ToSchema DateTime = 'SchemaScalar "DateTime"
  parseValue = parseValueScalar
