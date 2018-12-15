{-|
Module      :  MergeBot.Core.GraphQL.Scalars.DateTime
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the DateTime scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.DateTime where

import Data.GraphQL
import Data.GraphQL.Aeson (FromJSON)
import Data.Time (UTCTime)

newtype DateTime = DateTime { unDateTime :: UTCTime }
  deriving (Show,FromJSON)

instance GraphQLScalar DateTime

type instance ToScalar "DateTime" = DateTime

instance FromSchema DateTime where
  type ToSchema DateTime = 'SchemaScalar "DateTime"
  parseValue = parseValueScalar
