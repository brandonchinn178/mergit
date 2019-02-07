{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the backend server running a REST API.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Server (initApp) where

import Servant

import qualified MergeBot.Core as Core
import MergeBot.Core.Data
    (PullRequest, PullRequestDetail, PullRequestId, SessionInfo)
import MergeBot.Server.Monad

type MergeBotApi =
       "session" :> Get '[JSON] SessionInfo -- /session/          returns session information
  :<|> "pulls" :> PullRequestRoutes

type PullRequestRoutes =
       Get '[JSON] [PullRequest]            -- /pulls/            lists all PRs
  :<|> Capture "pr" PullRequestId :>
    (    Get '[JSON] PullRequestDetail      -- /pulls/:pr         gets the given PR
    :<|> "try" :> Post '[JSON] ()           -- /pulls/:pr/try     tries the given PR
    :<|> "queue" :> Post '[JSON] ()         -- /pulls/:pr/queue   queues the given PR
    :<|> "dequeue" :> Post '[JSON] ()       -- /pulls/:pr/dequeue dequeues the given PR
    )

initApp :: IO Application
initApp = serve (Proxy @MergeBotApi) . server <$> initEnv

server :: MergeBotEnv -> Server MergeBotApi
server env = hoistServer (Proxy @MergeBotApi) (runMergeBotHandler env) routes
  where
    routes =
      getSessionInfo
      :<|> listPullRequests
      :<|> (\prNum ->
        getPullRequest prNum
        :<|> tryPullRequest prNum
        :<|> queuePullRequest prNum
        :<|> unqueuePullRequest prNum
      )

{- Routes -}

getSessionInfo :: MergeBotHandler SessionInfo
getSessionInfo = Core.getSessionInfo

listPullRequests :: MergeBotHandler [PullRequest]
listPullRequests = Core.listPullRequests =<< getBotState

getPullRequest :: PullRequestId -> MergeBotHandler PullRequestDetail
getPullRequest prNum = flip Core.getPullRequest prNum =<< getBotState

tryPullRequest :: PullRequestId -> MergeBotHandler ()
tryPullRequest = Core.tryPullRequest

queuePullRequest :: PullRequestId -> MergeBotHandler ()
queuePullRequest pr =
  updateBotState_ $ \state -> do
    state' <- Core.queuePullRequest pr state
    base <- Core.getBaseBranch pr
    isMergeRunning <- Core.isMergeRunning base
    if isMergeRunning
      then Core.startMergeJob base state'
      else return state'

unqueuePullRequest :: PullRequestId -> MergeBotHandler ()
unqueuePullRequest = updateBotState_ . Core.unqueuePullRequest
