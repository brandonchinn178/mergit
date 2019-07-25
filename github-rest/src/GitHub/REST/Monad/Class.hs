{-|
Module      :  GitHub.REST.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'MonadGitHubREST' that gives a monad @m@ the capability to query the GitHub REST API.
-}
{-# LANGUAGE TypeApplications #-}

module GitHub.REST.Monad.Class
  ( MonadGitHubREST(..)
  , PageLinks(..)
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Aeson (FromJSON, Value)
import Data.Text (Text)

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
  {-# MINIMAL queryGitHubPage #-}

  queryGitHubPage :: FromJSON a => GHEndpoint -> m (a, PageLinks)

  queryGitHub :: FromJSON a => GHEndpoint -> m a
  queryGitHub = fmap fst . queryGitHubPage

  queryGitHub_ :: GHEndpoint -> m ()
  queryGitHub_ = void . queryGitHub @_ @Value

{- Instances for common monad transformers -}

instance MonadGitHubREST m => MonadGitHubREST (ReaderT r m) where
  queryGitHubPage = lift . queryGitHubPage

instance MonadGitHubREST m => MonadGitHubREST (ExceptT e m) where
  queryGitHubPage = lift . queryGitHubPage

instance MonadGitHubREST m => MonadGitHubREST (IdentityT m) where
  queryGitHubPage = lift . queryGitHubPage

instance MonadGitHubREST m => MonadGitHubREST (MaybeT m) where
  queryGitHubPage = lift . queryGitHubPage

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Lazy.RWST r w s m) where
  queryGitHubPage = lift . queryGitHubPage

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Strict.RWST r w s m) where
  queryGitHubPage = lift . queryGitHubPage

instance MonadGitHubREST m => MonadGitHubREST (Lazy.StateT s m) where
  queryGitHubPage = lift . queryGitHubPage

instance MonadGitHubREST m => MonadGitHubREST (Strict.StateT s m) where
  queryGitHubPage = lift . queryGitHubPage

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Lazy.WriterT w m) where
  queryGitHubPage = lift . queryGitHubPage

instance (Monoid w, MonadGitHubREST m) => MonadGitHubREST (Strict.WriterT w m) where
  queryGitHubPage = lift . queryGitHubPage

{- Pagination -}

data PageLinks = PageLinks
  { pageFirst :: Maybe Text
  , pagePrev  :: Maybe Text
  , pageNext  :: Maybe Text
  , pageLast  :: Maybe Text
  } deriving (Eq,Show)

instance Semigroup PageLinks where
  links1 <> links2 = PageLinks
    (pageFirst links1 <> pageFirst links2)
    (pagePrev links1 <> pagePrev links2)
    (pageNext links1 <> pageNext links2)
    (pageLast links1 <> pageLast links2)

instance Monoid PageLinks where
  mempty = PageLinks Nothing Nothing Nothing Nothing
