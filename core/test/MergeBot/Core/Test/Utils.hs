module MergeBot.Core.Test.Utils where

import qualified Data.Text as Text

import MergeBot.Core.GitHub (PaginatedResult(PaginatedResult))

-- | Get the paginated result of the given list and cursor.
paginated :: [a] -> Maybe String -> PaginatedResult a
paginated [] Nothing = PaginatedResult [] False Nothing
paginated xs cursor = case drop index xs of
  [] -> error $ "No data at index: " ++ show index
  (x:rest) ->
    let next = not $ null rest
    in PaginatedResult [x] next (if next then Just nextCursor else Nothing)
  where
    index = maybe 0 read cursor
    nextCursor = Text.pack $ show $ index + 1
