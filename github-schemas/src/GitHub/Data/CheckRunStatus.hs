{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Data.CheckRunStatus
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the CheckRunStatus enum.
-}
module GitHub.Data.CheckRunStatus where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum
  "CheckRunStatus"
  [ "QUEUED"
  , "IN_PROGRESS"
  , "COMPLETED"
  ]
