{-|
Module      :  GitHub.Data.CheckSuiteStatus
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the CheckSuiteStatus enum.
-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Data.CheckSuiteStatus where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "CheckSuiteStatus"
  [ "REQUESTED"
  , "QUEUED"
  , "IN_PROGRESS"
  , "COMPLETED"
  ]
