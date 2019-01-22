{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Test.Monad where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, gets)
import Data.GraphQL
    ( MonadQuery(..)
    , QueryT
    , mockedQuerySettings
    , runQuerySafeMocked
    , runQueryT
    )
import Data.GraphQL.TestUtils (mockWith)

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (BotEnv(..))
import MergeBot.Core.Test.Data (MockData)

-- | The monad to run tests
newtype TestApp a = TestApp { unTestApp :: StateT MockData (QueryT API IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState MockData
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

runTestApp :: TestApp a -> MockData -> IO a
runTestApp app mockData =
  runQueryT (mockedQuerySettings mockData)
  . (`evalStateT` mockData)
  . unTestApp
  $ app
