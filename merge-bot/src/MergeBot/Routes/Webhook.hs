{-|
Module      :  MergeBot.Routes.Webhook
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines webhook routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Webhook
  ( WebhookRoutes
  , handleWebhookRoutes
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN)
import Data.Aeson.Schema (IsSchema, get)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import qualified GitHub.Schema.Event.PullRequest as PullRequest
import GitHub.Schema.Repository (RepoWebhook)
import Servant
import Servant.GitHub
import UnliftIO.Exception (throwIO)

import MergeBot.Core.Actions (MergeBotAction(..), parseAction)
import MergeBot.Core.Error (BotError(..))
import MergeBot.Core.GitHub (PRForCommit(..), getPRForCommit)
import MergeBot.Core.Monad (getAppId)
import MergeBot.Core.Text (isStagingBranch, isTryBranch)
import MergeBot.Monad
    (BaseApp, BotApp, MergeBotEvent(..), ServerBase, queueEvent, runBotApp)

-- TODO: WithToken no longer needed?
type WebhookRoutes =
  GitHubEvent 'PingEvent :> GitHubAction
  :<|> GitHubEvent 'PullRequestEvent :> WithToken :> GitHubAction
  :<|> GitHubEvent 'CheckSuiteEvent :> WithToken :> GitHubAction
  :<|> GitHubEvent 'CheckRunEvent :> WithToken :> GitHubAction
  :<|> GitHubEvent 'StatusEvent :> WithToken :> GitHubAction
  :<|> GitHubEvent 'PushEvent :> WithToken :> GitHubAction

handleWebhookRoutes :: ServerBase WebhookRoutes
handleWebhookRoutes =
  handlePing
  :<|> handlePullRequest
  :<|> handleCheckSuite
  :<|> handleCheckRun
  :<|> handleStatus
  :<|> handlePush

-- | Handle the 'ping' GitHub event.
handlePing :: Object PingEvent -> BaseApp ()
handlePing o = liftIO $ putStrLn $ "Got ping from app #" ++ show [get| o.hook.app_id |]

-- | Handle the 'pull_request' GitHub event.
handlePullRequest :: Object PullRequestEvent -> Token -> BaseApp ()
handlePullRequest o = runBotApp' repo $ do
  logEvent "pull_request" o
  case [get| o.action |] of
    PullRequest.OPENED ->
      let (number, sha) = [get| o.pull_request.(number, head.sha) |]
      in queueEvent $ PRCreated number sha
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object CheckSuiteEvent -> Token -> BaseApp ()
handleCheckSuite o = runBotApp' repo $ do
  logEvent "check_suite" o
  appId <- getAppId
  when ([get| o.check_suite.app.id |] == appId) $
    case [get| o.action |] of
      CheckSuite.REQUESTED -> do
        pr <- getPRForCommit [get| o.check_suite.head_sha |]
        queueEvent $ CommitPushedToPR (prForCommitId pr) (prForCommitSHA pr)
      _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_run' GitHub event.
handleCheckRun :: Object CheckRunEvent -> Token -> BaseApp ()
handleCheckRun o = runBotApp' repo $ do
  logEvent "check_run" o
  case [get| o.action |] of
    CheckRun.REQUESTED_ACTION -> do
      pr <- getPRForCommit [get| o.check_run.head_sha |]

      let prNum = prForCommitId pr
          prBaseRef = prForCommitBaseBranch pr
          checkRunId = [get| o.check_run.id |]
          sha = prForCommitSHA pr
          action = [get| o.requested_action!.identifier |]

      case parseAction action of
        Just BotTry -> queueEvent $ StartTryJob prNum sha prBaseRef checkRunId
        Just BotQueue -> queueEvent $ QueuePR prNum sha
        Just BotDequeue -> queueEvent $ DequeuePR prNum sha
        Just BotResetMerge -> queueEvent $ ResetMerge prNum sha
        Nothing -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'status' GitHub event.
handleStatus :: Object StatusEvent -> Token -> BaseApp ()
handleStatus o = runBotApp' repo $ do
  logEvent "status" o
  case [get| o.branches[].name |] of
    [branch] | isTryBranch branch || isStagingBranch branch ->
      queueEvent $ RefreshCheckRun branch [get| o.sha |]
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'push' GitHub event.
handlePush :: Object PushEvent -> Token -> BaseApp ()
handlePush o = runBotApp' repo $ do
  logEvent "push" o
  when (isCreated && isCIBranch && not isBot) $ do
    queueEvent $ DeleteBranch branch
    throwIO $ CIBranchPushed o
  where
    repo = [get| o.repository! |]
    isCreated = [get| o.created |]
    isCIBranch = isStagingBranch branch || isTryBranch branch
    isBot = [get| o.sender.type |] == "Bot"
    branch = case Text.splitOn "/" [get| o.ref |] of
      [] -> error $ "Bad ref: " ++ Text.unpack [get| o.ref |]
      l -> last l

{- Helpers -}

-- | A helper around 'runBotAppT' for easy use by the Servant handlers.
runBotApp' :: Object RepoWebhook -> BotApp a -> Token -> BaseApp a
runBotApp' repo action token = runBotApp [get| repo.(owner.login, name) |] action token

-- | Log the given event for the given object.
logEvent :: IsSchema schema => Text -> Object schema -> BotApp ()
logEvent event o = logDebugN $
  "Received '" <> event <> "' event with payload: " <> Text.pack (show o)
