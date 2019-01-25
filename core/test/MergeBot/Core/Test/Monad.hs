{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Test.Monad where

import Control.Exception (try)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
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
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (StdMethod(..))

import MergeBot.Core.GitHub.REST (MonadGitHub(..), kvToValue, (.:))
import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (BotEnv(..))
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

instance MonadReader BotEnv TestApp where
  ask = pure $ BotEnv
    { repoOwner = "LeapYear"
    , repoName = "merge-bot-test"
    , ghToken = ""
    , ghManager = error "Tests do not use an HTTP manager"
    }
  local _ = id

instance MonadQuery API TestApp where
  runQuerySafe query args = runQuerySafeMocked query args =<< gets mockWith

instance MonadGitHub TestApp where
  queryGitHub method endpoint endpointVals ghData = case (endpoint, method) of
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

-- | Run the given action and then return either a server error or the final state at the end.
runTestApp' :: TestApp a -> MockData -> IO String
runTestApp' app = fmap (either show prettyState) . try @HttpException . runTestApp (app >> get)
