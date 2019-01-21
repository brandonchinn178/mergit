{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import MergeBot.Core (getSessionInfo)
import MergeBot.Core.Monad (runBot)
import MergeBot.Core.Test (goldens, testConfig)

import qualified Branch

main :: IO ()
main = defaultMain $ testGroup "MergeBot.Core"
  [ goldens "get_session_info" $ runBot testConfig getSessionInfo
  , Branch.tests
  ]
