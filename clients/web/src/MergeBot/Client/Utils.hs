{-# LANGUAGE LambdaCase #-}

module MergeBot.Client.Utils
  ( widgetFile
  , mkPrettyTime
  , renderBotStatus
  ) where

import Data.Default (def)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Language.Haskell.TH.Syntax (Exp, Q)
import Yesod (Html)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

import MergeBot.Client.Settings (appReloadTemplates)
import MergeBot.Client.StaticFiles.Icons
    (buffer_svg, checkmark_svg, play_svg, times_svg)
import MergeBot.Core.Data (BotStatus(..), MergeStatus(..), TryStatus(..))

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

-- | Render the given BotStatus.
renderBotStatus :: BotStatus -> (String, Maybe Html)
renderBotStatus = \case
  Merging MergeRunning -> ("merge-running", Just play_svg)
  Merging MergeFailed -> ("merge-failed", Just times_svg)
  MergeQueue -> ("merge-queue", Just buffer_svg)
  Trying TrySuccess -> ("try-success", Just checkmark_svg)
  Trying TryRunning -> ("try-running", Just play_svg)
  Trying TryFailed -> ("try-failed", Just times_svg)
  None -> ("none", Nothing)
