{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines helpers for querying the GitHub GraphQL API.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.GitHub
  ( graphqlSettings
  , queryAll
  ) where

import qualified Data.ByteString.Char8 as ByteString
import Data.GraphQL (QuerySettings(..), defaultQuerySettings)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (requestHeaders)
import Network.HTTP.Types (hAuthorization, hUserAgent)

-- | Settings to query GitHub's GraphQL endpoint
graphqlSettings :: String -> QuerySettings
graphqlSettings token = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  , modifyReq = \req -> req
      { requestHeaders =
          (hAuthorization, ByteString.pack $ "bearer " ++ token)
          : (hUserAgent, "LeapYear/merge-bot")
          : requestHeaders req
      }
  }

-- | Run a paginated query as many times as possible until all the results have been fetched.
queryAll :: Monad m => (Maybe String -> m ([a], Bool, Maybe Text)) -> m [a]
queryAll f = queryAll' Nothing
  where
    queryAll' cursor = do
      (result, hasNext, nextCursor) <- f $ Text.unpack <$> cursor
      next <- if hasNext
        then queryAll' $ Just $ fromJust nextCursor -- ensure Just to avoid infinite loop
        else return []
      return $ result ++ next
