module MergeBot.Client.Utils
  ( widgetFile
  , mkPrettyTime
  ) where

import Data.Default (def)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
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

{- Rendering functions -}

-- | Return a function that can convert a UTCTime into human-readable durations from the current
-- time; e.g. '2h ago'.
mkPrettyTime :: IO (UTCTime -> String)
mkPrettyTime = prettyTime <$> getCurrentTime
  where
    prettyTime now time = showDuration (now `diffUTCTime` time) ++ " ago"
    showDuration diff =
      let loop _ [] = error "mkPrettyTime: unreachable"
          loop x ((base, unit):rest) =
            case x `divMod` base of
              (0, r) -> show r ++ unit
              (q, _) -> loop q rest
      in loop (round diff) units
    units =
      [ (60, "s")
      , (60, "m")
      , (24, "h")
      , (7, "d")
      , (52, "w")
      , (inf, "y")
      ]
    inf = round (read "Infinity" :: Double) :: Integer
