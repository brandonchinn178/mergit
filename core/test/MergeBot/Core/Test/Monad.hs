{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Test.Monad where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, get, gets)
import Data.GraphQL
    ( MonadQuery(..)
    , QueryT
    , mockedQuerySettings
    , runQuerySafeMocked
    , runQueryT
    )
import Data.GraphQL.Aeson (FromJSON)
import Data.GraphQL.TestUtils (mockWith)
import Data.Text (Text)
import GitHub.REST (GHEndpoint(..), MonadGitHubREST(..), (.:))
import GitHub.REST.KeyValue (kvToValue)
import Network.HTTP.Types (StdMethod(..))

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (MonadBotApp(..))
import MergeBot.Core.Test.Mock

-- | The monad to run tests
newtype TestApp a = TestApp { unTestApp :: StateT MockState (QueryT API IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadState MockState
    , MonadThrow
    )

instance MonadBotApp TestApp where
  getRepo = pure ("LeapYear", "merge-bot-test")

instance MonadQuery API TestApp where
  runQuerySafe query args = runQuerySafeMocked query args =<< gets mockWith

instance MonadGitHubREST TestApp where
  queryGitHub GHEndpoint{..} = case (endpoint, method) of
    ("/repos/:owner/:repo/git/refs", POST) ->
      createBranch (ghData' "ref") (ghData' "sha")
    ("/repos/:owner/:repo/git/commits", POST) ->
      createCommit (ghData' "message") (ghData' "tree") (ghData' "parents")
    ("/repos/:owner/:repo/git/refs/:ref", DELETE) ->
      deleteBranch (endpointVals' "ref")
    ("/repos/:owner/:repo/merges", POST) ->
      mergeBranches (ghData' "base") (ghData' "head") (ghData' "message")
    ("/repos/:owner/:repo/git/refs/:ref", PATCH) ->
      updateBranch (endpointVals' "ref") (ghData' "sha")
    _ -> fail $ "Unhandled REST endpoint: " ++ show (endpoint, method)
    where
      endpointVals' :: FromJSON a => Text -> a
      endpointVals' = (kvToValue endpointVals .:)
      ghData' :: FromJSON a => Text -> a
      ghData' = (kvToValue ghData .:)

runTestApp :: TestApp a -> MockData -> IO a
runTestApp app mockData =
  runQueryT (mockedQuerySettings mockState)
  . (`evalStateT` mockState)
  . unTestApp
  $ app
  where
    mockState = toMockState mockData

newtype Result a = Result (a, MockState)

instance Show a => Show (Result a) where
  show (Result (result, state)) = "Result: " ++ show result ++ "\n\n" ++ prettyState state

-- | Run the given action and then return the result and the state.
runTestApp' :: TestApp a -> MockData -> IO (Result a)
runTestApp' app = runTestApp $ do
  result <- app
  state <- get
  return $ Result (result, state)
