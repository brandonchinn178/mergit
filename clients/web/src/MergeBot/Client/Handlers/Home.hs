{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Client.Handlers.Home
  ( getHomeR
  ) where

import Network.HTTP.Types (StdMethod(..))
import Yesod

import MergeBot.Client.App (Handler, callAPI)
import MergeBot.Client.Utils (widgetFile)
import MergeBot.Core.Data (PullRequest(..))

getHomeR :: Handler Html
getHomeR = do
  prs <- callAPI @[PullRequest] GET "/pulls/"
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")
