{-|
Module      :  MergeBot.Core.GraphQL.Scalars.GitObjectID
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitObjectID scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.GitObjectID where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..))
import Data.Text (Text)

newtype GitObjectID = GitObjectID { unOID :: Text }
  deriving (Show)

instance GraphQLScalar GitObjectID where
  getScalar = \case
    String s -> GitObjectID s
    v -> error $ "Invalid GitObjectID: " ++ show v

type instance ToScalar "GitObjectID" = GitObjectID

instance FromSchema GitObjectID where
  type ToSchema GitObjectID = 'SchemaScalar "GitObjectID"
  parseValue = parseValueScalar
