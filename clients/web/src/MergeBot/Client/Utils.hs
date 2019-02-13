module MergeBot.Client.Utils
  ( widgetFile
  ) where

import Data.Default (def)
import Language.Haskell.TH.Syntax (Exp, Q)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

import MergeBot.Client.Settings (appReloadTemplates)

-- | Automatically loads Hamlet, Cassius, Lucius, and Julius files for the given name in the
-- @templates/@ directory.
widgetFile :: String -> Q Exp
widgetFile = widgetFile' def
  where
    widgetFile' = if appReloadTemplates
      then widgetFileReload
      else widgetFileNoReload
