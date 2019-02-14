{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.StaticFiles.Icons where

import MergeBot.Client.Settings (compileTimeStaticDir)
import MergeBot.Client.StaticFiles.TH (iconFiles)

-- This generates easy references to any `.svg` files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
--
-- For example, to refer to the "checkmark.svg" SVG icon, you'd use:
--
--   checkmark_svg :: Html
iconFiles compileTimeStaticDir
