{-|
Module      :  MergeBot.Server.Event.Install
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the installation and installation_repositories GitHub events.
-}

module MergeBot.Server.Event.Install
  ( InstallApp
  , InstallRepo
  ) where

import Data.Aeson (Value(..))

import MergeBot.Server.Event.Class

-- | TODO: parse out values in constructors instead of just 'Value'
-- https://developer.github.com/v3/activity/events/types/#installationevent
data InstallApp = InstallApp Value

instance GitHubEvent InstallApp where
  parseEvent = return . InstallApp . Object
  runEvent _ = fail "install app"

-- | TODO: parse out values in constructors instead of just 'Value'
-- https://developer.github.com/v3/activity/events/types/#installationrepositoriesevent
data InstallRepo = InstallRepo Value

instance GitHubEvent InstallRepo where
  parseEvent = return . InstallRepo . Object
  runEvent _ = fail "install repo"
