{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the core functionality of the merge bot.
-}

module MergeBot.Core
  ( MonadGitHub(..)
  ) where

import MergeBot.Core.Monad.Class
