{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.Handlers.Home
  ( getHomeR
  ) where

import Yesod

import MergeBot.Client.App (Handler)
import MergeBot.Client.Utils (widgetFile)

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")
