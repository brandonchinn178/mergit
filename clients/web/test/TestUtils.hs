module TestUtils (withApp) where

import Test.Hspec
import Yesod.Test

import MergeBot.Client (initApp)
import MergeBot.Client.App (App)

withApp :: YSpec App -> Spec
withApp = before $ do
  app <- initApp
  return (app, id)
