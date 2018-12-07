{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for monads that can run GraphQL queries.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.GraphQL.Monad
  ( IsQueryable(..)
  , MonadQuery(..)
  , runQuery
  , QueryT
  , QuerySettings(..)
  , defaultQuerySettings
  , runQueryT
  , object
  -- * Re-exports
  , MonadIO
  ) where

import Control.Exception (throwIO)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Data.Text (Text)
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
import Data.GraphQL.Query.Internal (Query(..))
import Data.GraphQL.Result (GraphQLResult, getErrors, getResult)
import Data.GraphQL.Schema (Object(..))

-- | A helper for converting pairs into an Object.
object :: [Aeson.Pair] -> Aeson.Object
object = HashMap.fromList

-- | A type class for queryable results.
class IsQueryable result where
  type QueryArgs result = args | args -> result
  type ResultSchema result = schema | schema -> result
  fromArgs :: QueryArgs result -> Aeson.Object

-- | A type class for monads that can run queries.
class MonadIO m => MonadQuery m where
  runQuerySafe
    :: forall schema result
     . (IsQueryable result, schema ~ ResultSchema result)
    => Query schema -> QueryArgs result -> m (GraphQLResult (Object schema))

-- | Runs the given query and returns the result, erroring if the query returned errors.
runQuery
  :: forall m result schema
   . (MonadQuery m, IsQueryable result, schema ~ ResultSchema result)
  => Query schema -> QueryArgs result -> m (Object schema)
runQuery query args = do
  result <- runQuerySafe @_ @schema query args
  case getErrors result of
    [] -> return $ fromJust $ getResult @(Object schema) result
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
newtype QueryT m a = QueryT { unQueryT :: ReaderT QueryState m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadReader QueryState
    , MonadTrans
    )

instance MonadIO m => MonadQuery (QueryT m) where
  runQuerySafe
    :: forall schema result
     . (IsQueryable result, schema ~ ResultSchema result)
    => Query schema -> QueryArgs result -> QueryT m (GraphQLResult (Object schema))
  runQuerySafe (UnsafeQuery query) args = do
    state <- ask
    decodeResponse =<< case state of
      QueryState{..} ->
        let request = baseReq { requestBody = body }
        in liftIO $ responseBody <$> httpLbs request manager
      QueryMockState f ->
        return $ Aeson.encode $ Aeson.object
          [ "errors" .= ([] :: [GraphQLError])
          , "data" .= Just (f query args')
          ]
    where
      args' = fromArgs args
      body = RequestBodyLBS $ Aeson.encode $ Aeson.object
        [ "query" .= query
        , "variables" .= args'
        ]
      decodeResponse = either fail (traverse fromValue) . Aeson.eitherDecode
      fromValue = \case
        Aeson.Object o -> return $ UnsafeObject @schema o
        v -> fail $ "Could not decode GraphQL response: " ++ show v

-- | The settings for running QueryT.
data QuerySettings = QuerySettings
  { managerSettings :: ManagerSettings
    -- ^ Uses TLS by default
  , url             :: String
  , modifyReq       :: Request -> Request
  , mockResponse    :: Maybe (Text -> Aeson.Object -> Aeson.Value)
    -- ^ Instead of querying an API, use the given function to mock the response
  }

-- | Default query settings.
defaultQuerySettings :: QuerySettings
defaultQuerySettings = QuerySettings
  { managerSettings = tlsManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  , mockResponse = Nothing
  }

-- | The state for running QueryT.
data QueryState
  = QueryState
      { manager :: Manager
      , baseReq :: Request
      }
  | QueryMockState (Text -> Aeson.Object -> Aeson.Value)

-- | Run a QueryT stack.
runQueryT :: MonadIO m => QuerySettings -> QueryT m a -> m a
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
