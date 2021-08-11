{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Data.TeamPrivacy
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the TeamPrivacy enum.
-}
module GitHub.Data.TeamPrivacy where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "TeamPrivacy" ["SECRET", "CLOSED"]
