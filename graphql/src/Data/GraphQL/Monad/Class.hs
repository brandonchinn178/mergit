{-|
Module      :  Data.GraphQL.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'MonadQuery' type class, which allows GraphQL
queries to be run and mocked.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Monad.Class
  ( MonadQuery(..)
  , runQuery
  , runQuerySafeMocked
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Schema (IsSchemaObject, Object, SchemaType)
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (fromJust)

import Data.GraphQL.Error (GraphQLError, GraphQLException(..))
import Data.GraphQL.Query (GraphQLArgs(..), Query, queryName)
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)
import Data.GraphQL.TestUtils (MockedEndpoints, lookupMock)

-- | A type class for monads that can run queries.
class MonadIO m => MonadQuery (api :: k) m where
  runQuerySafe
    :: forall args (schema :: SchemaType)
     . (GraphQLArgs args, IsSchemaObject schema)
    => Query api args schema -> args -> m (GraphQLResult (Object schema))

-- | Runs the given query and returns the result, erroring if the query returned errors.
runQuery
  :: forall api m args (schema :: SchemaType)
   . (MonadQuery api m, GraphQLArgs args, IsSchemaObject schema)
  => Query api args schema -> args -> m (Object schema)
runQuery query args = do
  result <- runQuerySafe query args
  case getErrors result of
    [] -> return $ fromJust $ getResult result
    errors -> liftIO $ throwIO $ GraphQLException errors

-- | An implementation for mocked GraphQL endpoints using 'MockedEndpoints'.
runQuerySafeMocked
  :: forall api m args (schema :: SchemaType)
   . (Monad m, GraphQLArgs args, IsSchemaObject schema)
  => Query api args schema -> args -> MockedEndpoints api -> m (GraphQLResult (Object schema))
runQuerySafeMocked query args endpoints =
  case lookupMock query (fromArgs args) endpoints of
    Nothing -> fail $ "Endpoint missing mocked data: " ++ queryName query
    Just mockData ->
      either fail return $ Aeson.parseEither Aeson.parseJSON $ Aeson.object
        [ "errors" .= ([] :: [GraphQLError])
        , "data" .= Just mockData
        ]
