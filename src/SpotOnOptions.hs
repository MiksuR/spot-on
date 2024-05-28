module SpotOnOptions (
    getSpotOnOptions,
    Command (..),
    Params (..),
    Options (..)
  ) where

import Options.Applicative

data Command = Scroll | Previous | PlayPause | Next deriving Show
data Params = Params { textLength :: Int, speed :: Int } deriving Show
data Options = Options { function :: Command, params :: Params } deriving Show

paramsParser :: Parser Params
paramsParser = Params <$> lengthParser <*> speedParser
  where
    lengthParser = option auto (short 'l' <> long "length"
      <> help "How many characters should be printed"
      <> value 15 <> metavar "INT")
    speedParser = option auto (short 's' <> long "speed"
      <> help "How many milliseconds to wait before scrolling"
      <> value 500 <> metavar "INT")

commandParser :: Parser Command
commandParser = subparser $ command "scroll"
                  (info (pure Scroll)
                  (fullDesc <> progDesc "Print the current song while scrolling the text"))
               <> command "previous"
                  (info (pure Previous)
                  (fullDesc <> progDesc "Play previous song"))
               <> command "playpause"
                  (info (pure PlayPause)
                  (fullDesc <> progDesc "Toggle playback"))
               <> command "next"
                  (info (pure Next)
                  (fullDesc <> progDesc "Play next song"))

getSpotOnOptions :: IO Options
getSpotOnOptions = do
  let parser = (Options <$> commandParser <*> paramsParser) <**> helper
          <**> simpleVersioner "v1.0.0"
  let infoM = fullDesc <> progDesc "Spotify plugin for Polybar"
                       <> header "spot-on v1.0.0"
  execParser (info parser infoM)
