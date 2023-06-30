module Main (main) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.List.Extra ((!?))
import Data.Foldable (traverse_)
import DBus
import DBus.Client

data Options = Options { textLength :: Int, speed :: Int }

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
printSong :: Options -> IORef String -> Int -> IO ()
printSong os ref ind = do
  text <- readIORef ref
  let formatted = if length text >= textLength os
                  then take (textLength os) . drop ind $ cycle text
                  else text ++ replicate (textLength os - length text) ' '
  putStrLn formatted
  threadDelay . (1000*) . speed $ os
  let nextInd = if ind+1 < length text then ind+1 else 0
  printSong os ref nextInd

main :: IO ()
main = do
  -- TODO: Take command line arguments
  let opts = Options 10 500
  -- TODO: Check if Spotify is running and populate songRef
  -- with the corresponding value.
  songRef <- newIORef " - "
  client <- connectSession
  let match = matchAny {
    matchPath = Just $ objectPath_ "/org/mpris/MediaPlayer2",
    matchMember = Just $ memberName_ "PropertiesChanged"
  }
  _ <- addMatch client match (callback songRef)
  printSong opts songRef 0
