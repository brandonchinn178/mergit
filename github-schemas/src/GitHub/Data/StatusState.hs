{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Data.StatusState
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the StatusState enum.
-}
module GitHub.Data.StatusState where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum
  "StatusState"
  [ "ERROR"
  , "EXPECTED"
  , "FAILURE"
  , "PENDING"
  , "SUCCESS"
  ]
