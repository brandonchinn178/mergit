{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MergeBot.Client.Handlers where

import Yesod

import MergeBot.Client.App (App(..), Route(..), resourcesApp)
import MergeBot.Client.Handlers.Common
import MergeBot.Client.Handlers.Home

mkYesodDispatch "App" resourcesApp
