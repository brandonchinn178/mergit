{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines helpers for querying the GitHub GraphQL API.
-}

module MergeBot.Core.GitHub
  ( queryAll
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text

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
