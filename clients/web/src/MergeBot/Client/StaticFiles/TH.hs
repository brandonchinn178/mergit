{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.StaticFiles.TH (iconFiles) where

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath.Find (extension, fileName, find, (/=?), (==?))
import Yesod (Html, preEscapedToMarkup)

iconFiles :: FilePath -> Q [Dec]
iconFiles staticDir = getSvgFiles >>= concatMapM (generateIconFile staticDir)
  where
    getSvgFiles = runIO $ find (fileName /=? "tmp") (extension ==? ".svg") staticDir
    concatMapM f = fmap concat . mapM f

generateIconFile :: FilePath -> FilePath -> Q [Dec]
generateIconFile staticDir iconFilePath = do
  iconName <- newName . Text.unpack . toIconName . relFromStaticDir . Text.pack $ iconFilePath
  iconContent <- runIO $ readFile iconFilePath
  let body = normalB [| preEscapedToMarkup $(lift iconContent) |]
  sequence
    [ sigD iconName [t|Html|]
    , funD iconName [clause [] body []]
    ]
  where
    relFromStaticDir = fromJust . Text.stripPrefix (Text.pack staticDir)
    toIconName = Text.replace "." "_" . Text.replace "/" "_"
