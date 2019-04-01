{-|
Module      :  GitHub.Data.StatusState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the StatusState enum.
-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Data.StatusState where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "StatusState"
  [ "ERROR"
  , "EXPECTED"
  , "FAILURE"
  , "PENDING"
  , "SUCCESS"
  ]

-- | Summarize the given StatusStates as a single StatusState.
summarize :: [StatusState] -> StatusState
summarize states
  | not (null states) && all (== SUCCESS) states = SUCCESS
  | ERROR `elem` states = ERROR
  | FAILURE `elem` states = FAILURE
  | otherwise = PENDING
