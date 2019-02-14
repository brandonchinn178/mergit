{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Client.Handlers.Home
  ( getHomeR
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (sortBy)
import Network.HTTP.Types (StdMethod(..))
import Yesod

import MergeBot.Client.App (Handler, callAPI)
import MergeBot.Client.Utils
    (compareBotStatus, mkPrettyTime, renderBotStatus, widgetFile)
import MergeBot.Core.Data (PullRequest(..))

getHomeR :: Handler Html
getHomeR = do
  prs <- sortBy (compareBotStatus `on` status) <$> callAPI @[PullRequest] GET "/pulls/"
  prettyTime <- liftIO mkPrettyTime
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")
