{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Data.State
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the State enum.
-}
module GitHub.Data.State where

import Data.Aeson.Schema.TH (mkEnum)

mkEnum "State" ["OPEN", "CLOSED"]
