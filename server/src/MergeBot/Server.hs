{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the backend server running a REST API.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Server (initApp) where

import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.Trans.Control (liftBaseWith, restoreM)
import Servant

import qualified MergeBot.Core as Core
import MergeBot.Core.Data
    (PullRequest, PullRequestDetail, PullRequestId, SessionInfo)
import MergeBot.Core.State (BotState)
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
listPullRequests = Core.listPullRequests =<< getBotState'

getPullRequest :: PullRequestId -> MergeBotHandler PullRequestDetail
getPullRequest prNum = flip Core.getPullRequest prNum =<< getBotState'

tryPullRequest :: PullRequestId -> MergeBotHandler ()
tryPullRequest = Core.tryPullRequest

queuePullRequest :: PullRequestId -> MergeBotHandler ()
queuePullRequest = updateState' . Core.queuePullRequest

unqueuePullRequest :: PullRequestId -> MergeBotHandler ()
unqueuePullRequest = updateState' . Core.unqueuePullRequest

{- Helpers -}

updateState :: (BotState -> MergeBotHandler (BotState, a)) -> MergeBotHandler a
updateState runWithState = do
  stateMVar <- getBotState

  result <- liftBaseWith $ \runInBase ->
    modifyMVar stateMVar $ \state ->
      fmap (fromResult state) $ runInBase $ runWithState state

  restoreM result
  where
    fromResult state1 = \case
      Right (state2, a) -> (state2, Right a)
      Left err          -> (state1, Left err)

updateState' :: (BotState -> MergeBotHandler BotState) -> MergeBotHandler ()
updateState' f = updateState (fmap (, ()) . f)
