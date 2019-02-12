{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.Handlers.Common
  ( getFaviconR
  , getRobotsR
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Yesod

import MergeBot.Client.App (Handler)

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  typedContent "image/x-icon" $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = typedContent typePlain $(embedFile "config/robots.txt")

{- Helpers -}

typedContent :: ContentType -> ByteString -> Handler TypedContent
typedContent typ = return . TypedContent typ . toContent
