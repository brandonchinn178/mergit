{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines helpers for querying the GitHub API.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.GitHub
  (
  -- * GraphQL API
    PaginatedResult(..)
  , queryAll
  -- * REST API
  , MonadREST
  , runSimpleREST
  , createBranch
  , createCommit
  , createToken
  , deleteBranch
  , mergeBranches
  , updateBranch
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (StdMethod(..))

import MergeBot.Core.Monad (MonadBotApp, queryGitHub')

{- GraphQL API -}

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

type MonadREST m = (MonadBotApp m, MonadCatch m, MonadGitHubREST m)

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

instance MonadGitHubREST SimpleREST where
  getToken = asks fst
  getManager = asks snd
  getUserAgent = error "no user agent" -- TODO

runSimpleREST :: Token -> SimpleREST a -> IO a
runSimpleREST token (SimpleREST action) = do
  manager <- liftIO $ newManager tlsManagerSettings
  runReaderT action (token, manager)

-- | Create a branch.
--
-- https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadBotApp m => GitHubData -> m ()
createBranch ghData = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/refs"
  , endpointVals = []
  , ghData
  }

-- | Create a commit, returning the SHA of the created commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadBotApp m => GitHubData -> m Text
createCommit ghData = (.: "sha") <$> queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/commits"
  , endpointVals = []
  , ghData
  }

-- | Create a new installation token.
--
-- https://developer.github.com/v3/apps/#create-a-new-installation-token
createToken :: MonadBotApp m => Int -> m Text
createToken installationId = (.: "token") <$> queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/app/installations/:installation_id/access_tokens"
  , endpointVals = ["installation_id" := installationId]
  , ghData = []
  }

-- | Delete the given branch, ignoring the error if the branch doesn't exist.
--
-- https://developer.github.com/v3/git/refs/#delete-a-reference
deleteBranch :: MonadBotApp m => Text -> m ()
deleteBranch branch = void $ githubTry $ queryGitHub' GHEndpoint
  { method = DELETE
  , endpoint = "/repos/:owner/:repo/git/refs/:ref"
  , endpointVals = ["ref" := "heads/" <> branch]
  , ghData = []
  }

-- | Merge two branches, returning the merge commit information.
-- TODO: handle merge conflicts
--
-- https://developer.github.com/v3/repos/merging/#perform-a-merge
mergeBranches :: MonadBotApp m => GitHubData -> m ()
mergeBranches ghData = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/merges"
  , endpointVals = []
  , ghData
  }

-- | Set the given branch to the given commit. Returns false if update is not a fast-forward.
--
-- https://developer.github.com/v3/git/refs/#update-a-reference
updateBranch :: MonadBotApp m => Text -> GitHubData -> m Bool
updateBranch branch ghData = fmap isRight $ githubTry $ queryGitHub' GHEndpoint
  { method = PATCH
  , endpoint = "/repos/:owner/:repo/git/refs/:ref"
  , endpointVals = ["ref" := "heads/" <> branch]
  , ghData
  }
