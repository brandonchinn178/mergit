{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the backend server running a REST API.
-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Server (initApp) where

import Servant

import MergeBot.Server.Monad (MergeBotEnv, initEnv, runMergeBotHandler)

type MergeBotApi = EmptyAPI

server :: MergeBotEnv -> Server MergeBotApi
server env = hoistServer (Proxy @MergeBotApi) (runMergeBotHandler env) emptyServer

initApp :: IO Application
initApp = serve (Proxy @MergeBotApi) . server <$> initEnv
