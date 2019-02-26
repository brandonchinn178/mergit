{-|
Module      :  GitHub.REST.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'MonadGitHubREST' that gives a monad @m@ the capability to query the GitHub REST API.
-}

module GitHub.REST.Monad.Class
  ( MonadGitHubREST(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)

import GitHub.REST.Endpoint

-- | A type class for monads that can query the GitHub REST API.
--
-- Example:
--
-- > -- create the "foo" branch
-- > queryGitHub GHEndpoint
-- >   { method = POST
-- >   , endpoint = "/repos/:owner/:repo/git/refs"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     ]
-- >   , ghData =
-- >     [ "ref" := "refs/heads/foo"
-- >     , "sha" := "1234567890abcdef"
-- >     ]
-- >   }
--
-- It's recommended that you create functions for the API endpoints you're using:
--
-- > deleteBranch branch = queryGitHub GHEndpoint
-- >   { method = DELETE
-- >   , endpoint = "/repos/:owner/:repo/git/refs/:ref"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     , "ref" := "heads/" <> branch
-- >     ]
-- >   , ghData = []
-- >   }
class MonadIO m => MonadGitHubREST m where
  queryGitHub :: FromJSON a => GHEndpoint -> m a
