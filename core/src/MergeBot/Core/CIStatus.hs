{-|
Module      :  MergeBot.Core.CIStatus
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions related to the CI status of a branch.
-}
{-# LANGUAGE LambdaCase #-}

module MergeBot.Core.CIStatus where

import Data.Text (Text)

import MergeBot.Core.Data (CIStatus(..), JobStatus(..))
import MergeBot.Core.GraphQL.StatusState (StatusState(..))

-- | Convert the given status into a CIStatus.
toCIStatus :: [(Text, StatusState)] -> CIStatus
toCIStatus = CIStatus . map (fmap fromStatusState)
  where
    fromStatusState = \case
      EXPECTED -> CIWaiting
      ERROR -> CIFailed
      FAILURE -> CIFailed
      PENDING -> CIRunning
      SUCCESS -> CISuccess

-- | Return True if the given CI status is a successful CI status.
--
-- An empty CI status is not successful.
isSuccess :: CIStatus -> Bool
isSuccess (CIStatus statuses) = not (null statuses) && all ((== CISuccess) . snd) statuses

-- | Return True if the given CI status is a pending CI status.
--
-- An empty CI status is pending.
isPending :: CIStatus -> Bool
isPending (CIStatus statuses) = null statuses || any ((`elem` [CIRunning, CIWaiting]) . snd) statuses
