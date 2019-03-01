{-|
Module      :  MergeBot.Server.Event.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitHubEvent type class.
-}

module MergeBot.Server.Event.Class
  ( GitHubEvent(..)
  ) where

import Data.Aeson.Types (Object, Parser)

import MergeBot.Server.Monad (Handler)

class GitHubEvent e where
  -- | Parse a GitHub event from the given payload.
  parseEvent :: Object -> Parser e
  -- | The action to run when this event is received from GitHub.
  runEvent :: e -> Handler ()
