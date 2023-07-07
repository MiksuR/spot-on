module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.IORef
import Data.Maybe (fromMaybe)
import DBus.Client
import System.IO
import System.Process (callCommand)

import SpotOnOptions
import SpotifyCommunications

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
  songName <- getSong client
  songRef <- newIORef $ fromMaybe " - " songName
  -- Spawn a listener in a separate thread that
  -- listens for DBus `PropertiesChanged` signal.
  -- The callback updates the value in `songRef`.
  void $ setUpListener client songRef
  printSong ps songRef 0

playpause :: Client -> IO ()
playpause client = do
  callNoReply client (callSpotify "PlayPause")
  status <- getStatus client
  void $ case status of
    Just "Paused" -> callCommand "polybar-msg action \"#spot-on-playpause.hook.0\""
    Just "Playing" -> callCommand "polybar-msg action \"#spot-on-playpause.hook.1\""

main :: IO ()
main = do
  client <- connectSession
  opts <- getSpotOnOptions
  case opts of
    Options Scroll ps -> scroll client ps
    Options Previous _ -> callNoReply client (callSpotify "Previous")
    Options PlayPause _ -> playpause client
    Options Next _ -> callNoReply client (callSpotify "Next")
