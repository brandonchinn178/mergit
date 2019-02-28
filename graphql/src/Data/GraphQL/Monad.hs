{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for monads that can run GraphQL queries.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GraphQL.Monad
  ( MonadQuery(..)
  , runQuery
  , runQuerySafeMocked
  , QueryT
  , QuerySettings(..)
  , defaultQuerySettings
  , mockedQuerySettings
  , runQueryT
  -- * Re-exports
  , MonadIO
  ) where

import Control.Exception (throwIO)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control
    ( ComposeSt
    , MonadBaseControl(..)
    , MonadTransControl(..)
    , defaultLiftBaseWith
    , defaultLiftWith
    , defaultRestoreM
    , defaultRestoreT
    )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Schema (IsSchemaObject, Object, SchemaType)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Network.HTTP.Client
    ( Manager
    , ManagerSettings
    , Request(..)
    , RequestBody(..)
    , httpLbs
    , newManager
    , parseUrlThrow
    , responseBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType)

import Data.GraphQL.Error (GraphQLError, GraphQLException(..))
import Data.GraphQL.Query (GraphQLArgs(..), Query, fromQuery, queryName)
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)
import Data.GraphQL.TestUtils (MockedEndpoints, MocksApi(..), lookupMock)

-- | A type class for monads that can run queries.
class MonadIO m => MonadQuery api m where
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

-- | The monad transformer type that should be used to run GraphQL queries.
--
-- @
-- newtype MyMonad a = MyMonad { unMyMonad :: QueryT IO a }
--
-- runMyMonad :: MyMonad a -> IO a
-- runMyMonad = runQueryT querySettings . unMyMonad
--   where
--     querySettings = defaultQuerySettings
--       { url = "https://api.github.com/graphql"
--       , modifyReq = \\req -> req
--           { requestHeaders =
--               (hAuthorization, "bearer my_github_token") : requestHeaders req
--           }
--       }
-- @
newtype QueryT api m a = QueryT { unQueryT :: ReaderT (QueryState api) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadError e
    , MonadIO
    , MonadMask
    , MonadReader (QueryState api)
    , MonadThrow
    , MonadTrans
    )

instance MonadBase IO m => MonadBase IO (QueryT api m) where
  liftBase = liftBaseDefault

instance MonadTransControl (QueryT api) where
  type StT (QueryT api) a = StT (ReaderT (QueryState api)) a
  liftWith = defaultLiftWith QueryT unQueryT
  restoreT = defaultRestoreT QueryT

instance MonadBaseControl IO m => MonadBaseControl IO (QueryT api m) where
  type StM (QueryT api m) a = ComposeSt (QueryT api) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadIO m => MonadQuery api (QueryT api m) where
  runQuerySafe query args = ask >>= \case
    QueryState{..} ->
      let request = baseReq
            { requestBody = RequestBodyLBS $ Aeson.encode $ Aeson.object
                [ "query" .= fromQuery query
                , "variables" .= fromArgs args
                ]
            }
      in liftIO $ decodeResponse . responseBody =<< httpLbs request manager
    QueryMockState endpoints -> runQuerySafeMocked query args endpoints

-- | An implementation for mocked GraphQL endpoints using MockedEndpoints and MocksApi.
runQuerySafeMocked
  :: forall api m args (schema :: SchemaType)
   . (Monad m, GraphQLArgs args, IsSchemaObject schema)
  => Query api args schema -> args -> MockedEndpoints api -> m (GraphQLResult (Object schema))
runQuerySafeMocked query args endpoints =
  decodeResponse =<< case lookupMock query (fromArgs args) endpoints of
    Nothing -> fail $ "Endpoint missing mocked data: " ++ queryName query
    Just mockData ->
      return $ Aeson.encode $ Aeson.object
        [ "errors" .= ([] :: [GraphQLError])
        , "data" .= Just mockData
        ]

-- | Decode a GraphQL response.
decodeResponse
  :: forall (schema :: SchemaType) m
   . (Monad m, IsSchemaObject schema)
  => ByteString -> m (GraphQLResult (Object schema))
decodeResponse = either fail return . Aeson.eitherDecode

-- | The settings for running QueryT.
data QuerySettings api = QuerySettings
  { managerSettings :: ManagerSettings
    -- ^ Uses TLS by default
  , url             :: String
  , modifyReq       :: Request -> Request
  , mockResponse    :: Maybe (MockedEndpoints api)
    -- ^ Instead of querying an API, use the given function to mock the response
  }

-- | Default query settings.
defaultQuerySettings :: QuerySettings api
defaultQuerySettings = QuerySettings
  { managerSettings = tlsManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  , mockResponse = Nothing
  }

-- | Query settings for mocking endpoints.
mockedQuerySettings :: MocksApi api mock => mock -> QuerySettings api
mockedQuerySettings mock = defaultQuerySettings { mockResponse = Just $ mockWith mock }

-- | The state for running QueryT.
data QueryState api
  = QueryState
      { manager :: Manager
      , baseReq :: Request
      }
  | QueryMockState (MockedEndpoints api)

-- | Run a QueryT stack.
runQueryT :: MonadIO m => QuerySettings api -> QueryT api m a -> m a
runQueryT QuerySettings{..} query = do
  state <- case mockResponse of
    Nothing -> liftIO $ do
      manager <- newManager managerSettings
      baseReq <- modifyReq . modifyReq' <$> parseUrlThrow url
      return QueryState{..}
    Just mockResp -> return $ QueryMockState mockResp
  (`runReaderT` state)
    . unQueryT
    $ query
  where
    modifyReq' req = req
      { method = "POST"
      , requestHeaders = (hContentType, "application/json") : requestHeaders req
      }
