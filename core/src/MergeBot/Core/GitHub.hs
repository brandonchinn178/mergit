{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines helpers for querying the GitHub API.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.GitHub
  (
  -- * GraphQL API
    graphqlSettings
  , queryAll
  -- * REST API
  , createBranch
  , createCommit
  , deleteBranch
  , mergeBranches
  -- * Re-exports
  , module MergeBot.Core.GitHub.REST
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as ByteString
import Data.GraphQL (QuerySettings(..), defaultQuerySettings)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (requestHeaders)
import Network.HTTP.Types (StdMethod(..), hAuthorization, hUserAgent, status422)

import MergeBot.Core.GitHub.REST

{- GraphQL API -}

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

{- REST API -}

-- | Create a branch.
--
-- https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadGitHub m => GitHubData -> m ()
createBranch = void . queryGitHub POST "/repos/:owner/:repo/git/refs" []

-- | Create a commit, returning the SHA of the created commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadGitHub m => GitHubData -> m Text
createCommit = fmap (.: "sha") . queryGitHub POST "/repos/:owner/:repo/git/commits" []

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
-- https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: (MonadCatch m, MonadGitHub m) => Text -> m ()
deleteBranch branch = void $ handleStatus status422 $
  queryGitHub DELETE "/repos/:owner/:repo/git/refs/:ref" ["ref" := "heads/" <> branch] []

-- | Merge two branches, returning the merge commit information.
-- TODO: handle merge conflicts
--
-- https://developer.github.com/v3/repos/merging/#perform-a-merge
mergeBranches :: MonadGitHub m => GitHubData -> m Value
mergeBranches = fmap (.: "commit") . queryGitHub POST "/repos/:owner/:repo/merges" []
