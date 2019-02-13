module MergeBot.Client.CommandLine
  ( AppOptions(..)
  , parseOptions
  ) where

import Options.Applicative

data AppOptions = AppOptions
  { configFile :: FilePath
  } deriving (Show)

parseOptions :: IO AppOptions
parseOptions = execParser (parseOptions' `withInfo` description)
  where
    withInfo opts = info (helper <*> opts) . progDesc
    description = "LeapYear Merge Bot"
    parseOptions' = AppOptions <$> parseConfigFile
    parseConfigFile = strOption $ mconcat
      [ long "config"
      , short 'c'
      , metavar "CONFIG"
      , help "Path to the configuration file"
      , value "settings.yaml"
      , showDefault
      ]
