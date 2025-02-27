{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | An example usage of a Servant website that allows GitHub events at the '/webhook/' route.
module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (get)
import Data.Text qualified as Text
import GitHub.Data.URL (URL (..))
import GitHub.REST (
  GHEndpoint (..),
  GitHubSettings (..),
  KeyValue (..),
  queryGitHub,
  runGitHubT,
 )
import GitHub.Schema.Repository (RepoWebhook)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub

type ExampleGitHubEvents =
  GitHubEvent 'InstallationEvent :> WithToken :> GitHubAction
    :<|> GitHubAction

type ExampleApp =
  Get '[PlainText] String
    :<|> "webhook" :> ExampleGitHubEvents

getHelloWorld :: Handler String
getHelloWorld = pure "Hello world"

handleInstallationEvent :: Object InstallationEvent -> Token -> Handler ()
handleInstallationEvent o token = liftIO $ do
  putStrLn "** Got installation event!"
  putStrLn $ "Installation ID: " ++ show [get| o.installation.id |]

  GitHubAppParams{ghUserAgent} <- loadGitHubAppParams
  let settings = GitHubSettings (Just token) ghUserAgent "v3"

  forM_ [get| o.repositories[].full_name |] $ \repoName -> do
    repo <-
      runGitHubT settings $
        queryGitHub @_ @(Object RepoWebhook)
          GHEndpoint
            { method = GET
            , endpoint = "/repos/:full_repo_name"
            , endpointVals = ["full_repo_name" := repoName]
            , ghData = []
            }
    putStrLn $ "Repository name: " ++ Text.unpack repoName
    putStrLn $ "URL: " ++ (Text.unpack . unURL $ [get| repo.html_url |])
    putStrLn $ "Description: " ++ maybe "N/A" Text.unpack [get| repo.description |]

handleGitHubEvent :: Handler ()
handleGitHubEvent = liftIO $ putStrLn "** Got GitHub event!"

server :: Server ExampleApp
server = getHelloWorld :<|> (handleInstallationEvent :<|> handleGitHubEvent)

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  pure $ serveWithContext (Proxy @ExampleApp) (params :. EmptyContext) server

main :: IO ()
main = run 3000 =<< initApp
