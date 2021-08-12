{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.GitHub.CombinatorsTest (test) where

import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (methodPost)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import Servant
import Servant.GitHub (GitHubAppParams (..), GitHubEvent, GitHubEventType (..))
import Servant.GitHub.Security (sha1sum)
import Test.Tasty
import Test.Tasty.HUnit
import Web.JWT (Signer (..))

test :: TestTree
test =
  testGroup
    "Servant.GitHub.Combinators"
    [ testGitHubEvent
    ]

type EventTestAPI =
  GitHubEvent 'PingEvent :> Post '[PlainText] Text
    :<|> GitHubEvent 'DeleteEvent :> Post '[PlainText] Text

eventTestRoutes :: Server EventTestAPI
eventTestRoutes = (\_ -> pure "ping") :<|> (\_ -> pure "delete")

testGitHubEvent :: TestTree
testGitHubEvent =
  testGroup
    "GitHubEvent"
    [ testCaseSteps "routes by event" $ \step -> do
        step "Test ping event"
        runSession $ do
          resp <- sendEvent "ping" pingPayload
          Wai.assertBody "ping" resp
          Wai.assertStatus 200 resp
        step "Test delete event"
        runSession $ do
          resp <- sendEvent "delete" deletePayload
          Wai.assertBody "delete" resp
          Wai.assertStatus 200 resp
    , testCaseSteps "failure to decode request body immediately aborts" $ \step -> do
        step "Test ping event"
        runSession $ do
          resp <- sendEvent "ping" [aesonQQ|{}|]
          Wai.assertStatus 400 resp
          Wai.assertBodyContains "Could not decode" resp
        step "Test delete event"
        runSession $ do
          resp <- sendEvent "delete" [aesonQQ|{}|]
          Wai.assertStatus 400 resp
          Wai.assertBodyContains "Could not decode" resp
    ]
  where
    webhookSecret = "asdf"
    runSession m = do
      let context = ghAppParams :. EmptyContext
          ghAppParams =
            GitHubAppParams
              { ghAppId = 1
              , ghWebhookSecret = webhookSecret
              , ghSigner = HMACSecret ""
              , ghUserAgent = ""
              }
      Wai.runSession m $ serveWithContext (Proxy @EventTestAPI) context eventTestRoutes
    sendEvent evt payload = do
      let body = encode payload
      Wai.srequest $
        Wai.SRequest
          { Wai.simpleRequest =
              Wai.defaultRequest
                { Wai.requestMethod = methodPost
                , Wai.requestHeaders =
                    [ ("x-github-event", evt)
                    , ("x-hub-signature", hash body)
                    ]
                }
          , Wai.simpleRequestBody = body
          }
    hash = Text.encodeUtf8 . ("sha1=" <>) . Text.pack . show . sha1sum webhookSecret . ByteStringL.toStrict

    pingPayload =
      [aesonQQ|
      {
        "zen": "foo",
        "hook": {
          "app_id": 1
        }
      }
    |]

    deletePayload =
      [aesonQQ|
      {
        "ref_type": "BRANCH",
        "ref": "asdf",
        "sender": {
          "login": "user1",
          "id": 1,
          "node_id": "",
          "avatar_url": "",
          "gravatar_id": "",
          "url": "",
          "html_url": "",
          "followers_url": "",
          "following_url": "",
          "gists_url": "",
          "starred_url": "",
          "subscriptions_url": "",
          "organizations_url": "",
          "repos_url": "",
          "events_url": "",
          "received_events_url": "",
          "type": "",
          "site_admin": false
        },
        "repository": null,
        "organization": null,
        "installation": null
      }
    |]
