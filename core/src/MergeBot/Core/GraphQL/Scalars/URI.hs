{-|
Module      :  MergeBot.Core.GraphQL.Scalars.URI
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the URI scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.URI where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..))
import Data.Text (Text)

newtype URI = URI { unURI :: Text }
  deriving (Show)

instance GraphQLScalar URI where
  getScalar = \case
    String s -> URI s
    v -> error $ "Invalid URI: " ++ show v

type instance ToScalar "URI" = URI

instance FromSchema URI where
  type ToSchema URI = 'SchemaScalar "URI"
  parseValue = parseValueScalar
