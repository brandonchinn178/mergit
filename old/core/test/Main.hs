{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import MergeBot.Core (getSessionInfo)
import MergeBot.Core.Test (goldens, initialState, runTestApp)

import qualified Branch

main :: IO ()
main = defaultMain $ testGroup "MergeBot.Core"
  [ goldens "get_session_info" $ runTestApp getSessionInfo initialState
  , Branch.tests
  ]
