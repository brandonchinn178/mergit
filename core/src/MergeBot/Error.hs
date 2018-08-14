{-|
Module      :  MergeBot.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines errors that can be thrown in the merge bot.
-}

module MergeBot.Error
  ( BotError(..)
  ) where

-- | Errors that may occur in modifying the state of the merge bot.
data BotError
  = AlreadyInMergeQueue -- ^ Cannot add to merge queue twice
  | PatchNotApproved    -- ^ Cannot add to merge queue if it's approved
  | MergeJobStarted     -- ^ Cannot remove from merge queue if PR already running
  | TryJobStarted       -- ^ Cannot start a try job if PR already running a try job
  | DoesNotExist        -- ^ The PR does not exist
  deriving (Show,Eq)
