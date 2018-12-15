{-|
Module      :  MergeBot.Core.GraphQL.Scalars.URI
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the URI scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.URI where

import Data.GraphQL
import Data.GraphQL.Aeson (FromJSON)
import Data.Text (Text)

newtype URI = URI { unURI :: Text }
  deriving (Show,FromJSON)

instance GraphQLScalar URI

type instance ToScalar "URI" = URI

instance FromSchema URI where
  type ToSchema URI = 'SchemaScalar "URI"
  parseValue = parseValueScalar
