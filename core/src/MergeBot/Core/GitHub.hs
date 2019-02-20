{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines helpers for querying the GitHub API.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.GitHub
  (
  -- * GraphQL API
    graphqlSettings
  , PaginatedResult(..)
  , queryAll
  -- * REST API
  , runSimpleREST
  , createBranch
  , createCommit
  , createToken
  , deleteBranch
  , mergeBranches
  , updateBranch
  -- * Re-exports
  , module MergeBot.Core.GitHub.REST
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import qualified Data.ByteString.Char8 as ByteString
import Data.Either (isRight)
import Data.GraphQL (QuerySettings(..), defaultQuerySettings)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (Manager, newManager, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (StdMethod(..), hAuthorization, hUserAgent)

import MergeBot.Core.GitHub.REST
import MergeBot.Core.GraphQL.API (API)

{- GraphQL API -}

-- | Settings to query GitHub's GraphQL endpoint
graphqlSettings :: String -> QuerySettings API
graphqlSettings token = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  , modifyReq = \req -> req
      { requestHeaders =
          (hAuthorization, ByteString.pack $ "bearer " ++ token)
          : (hUserAgent, "LeapYear/merge-bot")
          : requestHeaders req
      }
  }

data PaginatedResult a = PaginatedResult
  { chunk      :: [a]
  , hasNext    :: Bool
  , nextCursor :: Maybe Text
  }

-- | Run a paginated query as many times as possible until all the results have been fetched.
queryAll :: Monad m => (Maybe String -> m (PaginatedResult a)) -> m [a]
queryAll doQuery = queryAll' Nothing
  where
    queryAll' cursor = do
      PaginatedResult{..} <- doQuery cursor
      next <- if hasNext
        -- ensure Just to avoid infinite loop
        then queryAll' . Just . Text.unpack . fromJust $ nextCursor
        else return []
      return $ chunk ++ next

{- REST API -}

-- | A simple monad that can run REST calls.
newtype SimpleREST a = SimpleREST (ReaderT (Token, Manager) IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadIO
    , MonadReader (Token, Manager)
    , MonadThrow
    )

instance MonadGitHub SimpleREST where
  getToken = asks fst
  getManager = asks snd

runSimpleREST :: Token -> SimpleREST a -> IO a
runSimpleREST token (SimpleREST action) = do
  manager <- liftIO $ newManager tlsManagerSettings
  runReaderT action (token, manager)

-- | Create a branch.
--
-- https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadREST m => GitHubData -> m ()
createBranch = void . queryGitHub POST "/repos/:owner/:repo/git/refs" []

-- | Create a commit, returning the SHA of the created commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadREST m => GitHubData -> m Text
createCommit = fmap (.: "sha") . queryGitHub POST "/repos/:owner/:repo/git/commits" []

-- | Create a new installation token.
--
-- https://developer.github.com/v3/apps/#create-a-new-installation-token
createToken :: MonadREST m => Int -> m Text
createToken installationId = (.: "token") <$>
  queryGitHub POST "/app/installations/:installation_id/access_tokens"
    ["installation_id" :=* installationId] []

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
-- https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: MonadREST m => Text -> m ()
deleteBranch branch = void $ githubTry $
  queryGitHub DELETE "/repos/:owner/:repo/git/refs/:ref" ["ref" := "heads/" <> branch] []

-- | Merge two branches, returning the merge commit information.
-- TODO: handle merge conflicts
--
-- https://developer.github.com/v3/repos/merging/#perform-a-merge
mergeBranches :: MonadREST m => GitHubData -> m ()
mergeBranches = void . queryGitHub POST "/repos/:owner/:repo/merges" []

-- | Set the given branch to the given commit. Returns false if update is not a fast-forward.
--
-- https://developer.github.com/v3/git/refs/#update-a-reference
updateBranch :: MonadREST m => Text -> GitHubData -> m Bool
updateBranch branch = fmap isRight . githubTry .
  queryGitHub PATCH "/repos/:owner/:repo/git/refs/:ref" ["ref" := "heads/" <> branch]
