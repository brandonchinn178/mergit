{-|
Module      :  MergeBot.Server.Event
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines events that can come from GitHub.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Server.Event
  ( parseEventHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types (Object, Parser)
import Data.Text (Text)
import qualified Data.Text as Text

import MergeBot.Server.Event.Class
import MergeBot.Server.Event.Install
import MergeBot.Server.Monad (Handler)

-- | Return a parser that returns the action to run for the given GitHub event.
parseEventHandler :: Object -> Text -> Parser (Handler ())
parseEventHandler o = \case
  "installation"              -> parse @InstallApp
  "installation_repositories" -> parse @InstallRepo
  -- TODO: other events
  event -> return $ liftIO $
    -- TODO: better logging
    putStrLn $ "Ignoring event: " ++ Text.unpack event
  where
    parse :: forall e. GitHubEvent e => Parser (Handler ())
    parse = runEvent <$> parseEvent @e o
