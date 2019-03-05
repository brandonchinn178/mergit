-- | An example usage of a Servant website that allows GitHub events at the '/webhook/' route.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub

type ExampleGitHubEvents
  = GitHubEvent 'InstallationEvent :> Post '[JSON] ()
  :<|> Post '[JSON] ()

type ExampleApp
  = Get '[PlainText] String
  :<|> "webhook" :> GitHubSigned :> ExampleGitHubEvents

getHelloWorld :: Handler String
getHelloWorld = pure "Hello world"

handleInstallationEvent :: Object InstallationSchema -> Handler ()
handleInstallationEvent o = liftIO $ putStrLn $ "Got installation event: " ++ show o

handleGitHubEvent :: Handler ()
handleGitHubEvent = liftIO $ putStrLn "Got GitHub event!"

server :: Server ExampleApp
server = getHelloWorld :<|> (handleInstallationEvent :<|> handleGitHubEvent)

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @ExampleApp) (params :. EmptyContext) server

main :: IO ()
main = run 3000 =<< initApp
