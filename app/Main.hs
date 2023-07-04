module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe)
import DBus
import DBus.Client
import Options.Applicative
import System.Cmd (system)
import System.IO

data Command = Scroll | Previous | PlayPause | Next deriving Show
data Params = Params { textLength :: Int, speed :: Int } deriving Show
data Options = Options { function :: Command, params :: Params } deriving Show

-- Writes the name of the current song in an IORef
-- I know this code will break if Spotify changes the
-- signal format, but I'll improve the code later.
callback :: IORef String -> Signal -> IO ()
callback ref sig = traverse_ (writeIORef ref) maybeSongText
  where
    maybeSongText = do
      body <- signalBody sig !? 1
      mData <- fromVariant body :: Maybe Dictionary
      let dataVariant = snd . head $ dictionaryItems mData
      dict <- fromVariant dataVariant >>= fromVariant :: Maybe Dictionary
      artistsVariant <- dictionaryItems dict !? 4
      titleVariant <- dictionaryItems dict !? 8
      artists <- fromVariant (snd artistsVariant) >>= fromVariant :: Maybe [String]
      title <- fromVariant (snd titleVariant) >>= fromVariant :: Maybe String
      artist <- artists !? 0
      return $ title ++ " - " ++ artist ++ " - "

-- Infinitely looping function that prints the song name in
-- the terminal while scrolling it.
printSong :: Params -> IORef String -> Int -> IO ()
printSong ps ref ind = do
  text <- readIORef ref
  let formatted = if length text >= textLength ps
                  then take (textLength ps) . drop ind $ cycle text
                  else text ++ replicate (textLength ps - length text) ' '
  putStrLn formatted
  threadDelay . (1000*) . speed $ ps
  let nextInd = if ind+1 < length text then ind+1 else 0
  printSong ps ref nextInd

scroll :: Client -> Params -> IO ()
scroll client ps = do
  hSetBuffering stdout LineBuffering
  songMData <- getProperty client (callSpotify "Metadata")
  let songName = do
        mVariant <- eitherToMaybe songMData
        dict <- fromVariant mVariant :: Maybe Dictionary
        artistsVariant <- dictionaryItems dict !? 4
        titleVariant <- dictionaryItems dict !? 8
        artists <- fromVariant (snd artistsVariant) >>= fromVariant :: Maybe [String]
        title <- fromVariant (snd titleVariant) >>= fromVariant :: Maybe String
        artist <- artists !? 0
        return $ title ++ " - " ++ artist ++ " - "
  songRef <- newIORef $ fromMaybe " - " songName
  let match = matchAny {
    matchPath = Just $ objectPath_ "/org/mpris/MediaPlayer2",
    matchMember = Just $ memberName_ "PropertiesChanged"
  }
  _ <- addMatch client match (callback songRef)
  printSong ps songRef 0

playpause :: Client -> IO ()
playpause client = do
  callNoReply client (callSpotify "PlayPause")
  status <- getProperty client (callSpotify "PlaybackStatus")
  let statusString = do
        statusVariant <- eitherToMaybe status
        fromVariant statusVariant :: Maybe String
  _ <- case statusString of
    Just "Paused" -> system "polybar-msg action \"#spot-on-playpause.hook.0\""
    Just "Playing" -> system "polybar-msg action \"#spot-on-playpause.hook.1\""
  return ()

paramsParser :: Parser Params
paramsParser = Params <$> lengthParser <*> speedParser
  where
    lengthParser = option auto (short 'l'
      <> help "How many characters should be printed"
      <> value 10)
    speedParser = option auto (short 's'
      <> help "How many characters/millisecond should be scrolled"
      <> value 800)

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

callSpotify :: String -> MethodCall
callSpotify member =
  (methodCall
   (objectPath_ "/org/mpris/MediaPlayer2")
   (interfaceName_ "org.mpris.MediaPlayer2.Player")
   (memberName_ member))
  {methodCallDestination =
   Just $ busName_ "org.mpris.MediaPlayer2.spotify"}

main :: IO ()
main = do
  let parser = (Options <$> commandParser <*> paramsParser) <**> helper
  let infoM = fullDesc <> header "Help (spot-on)"
                       <> progDesc "Spotify plugin for Polybar"
  opts <- execParser (info parser infoM)
  client <- connectSession
  case opts of
    Options Scroll ps -> scroll client ps
    Options Previous _ -> callNoReply client (callSpotify "Previous")
    Options PlayPause _ -> playpause client
    Options Next _ -> callNoReply client (callSpotify "Next")
