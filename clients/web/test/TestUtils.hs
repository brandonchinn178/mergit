module TestUtils (withApp) where

import Test.Hspec
import Yesod.Test

import MergeBot.Client (initApp)
import MergeBot.Client.App (App)
import MergeBot.Client.Settings (loadAppSettings)

withApp :: YSpec App -> Spec
withApp = before $ do
  appSettings <- loadAppSettings "config/settings.dev.yaml"
  app <- initApp appSettings
  return (app, id)
