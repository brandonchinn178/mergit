{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}

module MergeBot.Core.Test.Monad where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.GraphQL

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (BotAppT(..), BotEnv(..))
import MergeBot.Core.Test.Data (MockData)

-- | The monad to run tests
newtype TestApp a = TestApp (BotAppT IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader BotEnv
    , MonadQuery API
    )

runTestApp :: TestApp a -> MockData -> IO a
runTestApp (TestApp botApp) mock =
  runQueryT (mockedQuerySettings mock) $ runReaderT (unBotApp botApp) botEnv
  where
    botEnv = BotEnv
      { repoOwner = "LeapYear"
      , repoName = "merge-bot-test"
      , ghToken = ""
      , ghManager = error "Tests do not use an HTTP manager"
      }
