{-# LANGUAGE CPP #-}

-- | Creating a separate module because stylish-haskell doesn't like CPP macros, and if I'm going
-- to make stylish-haskell ignore a file, I want that file to be as minimal as possible.
module MergeBot.Client.Settings.Development where

isDevelopment :: Bool
#ifdef DEVELOPMENT
isDevelopment = True
#else
isDevelopment = False
#endif
