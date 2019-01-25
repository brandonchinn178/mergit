{-|
Module      :  Data.GraphQL.TestUtils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines test utilities for testing GraphQL queries.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Data.GraphQL.TestUtils
  ( MockedEndpoints
  , MocksApi(..)
  , lookupMock
  , mock
  ) where

import Data.Aeson (Object, Value)
import Data.Maybe (listToMaybe)

import Data.GraphQL.Query (Query, fromQuery)

-- | A mapping between queries and the callback to run when the endpoint for the given query is
-- accessed.
--
-- The first value should be of the form `mock Foo.query`.
--
-- The callback takes in the arguments passed to the endpoint and returns the full JSON result that
-- would be sent back by the actual API.
type MockedEndpoints api = [(Query' api, Object -> Value)]

-- | A type class for a type that mocks endpoints for the given API.
class MocksApi api mock where
  -- | Use the given mock data to build endpoints that will be used for all API calls.
  mockWith :: mock -> MockedEndpoints api

-- | Lookup the given query in the given endpoints and apply the arguments to get the mocked result.
lookupMock :: Query api args schema -> Object -> MockedEndpoints api -> Maybe Value
lookupMock query args = fmap (($ args) . snd) . listToMaybe . filter (matches query . fst)
  where
    matches :: Query api args schema -> Query' api -> Bool
    matches q1 (Query' q2) = fromQuery q1 == fromQuery q2

-- | A 'Query' that has forgotten its arguments and schema.
data Query' api = forall args schema. Query' (Query api args schema)

-- | A helper to build a list of 'Query' values that don't care about the result type anymore.
mock :: Query api args schema -> Query' api
mock = Query'
