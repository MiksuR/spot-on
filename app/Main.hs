module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.IORef
import Data.Maybe (fromMaybe)
import DBus.Client
import System.IO

import SpotOnOptions
import SpotifyCommunications

-- Infinitely looping function that prints the song name in
-- the terminal while scrolling it.
printSong :: Params -> IORef (Maybe String) -> Int -> IO ()
printSong ps ref ind = do
  maybeText <- readIORef ref
  let formatted = do
        text <- maybeText
        if length text >= textLength ps
        then return $ take (textLength ps) . drop ind $ cycle text
        else return $ text ++ replicate (textLength ps - length text) ' '
  putStrLn . fromMaybe " - " $ formatted
  threadDelay . (1000*) . speed $ ps
  -- TODO: Do something smarter here v
  let nextInd = if ind+1 < length (fromMaybe "" maybeText) then ind+1 else 0
  printSong ps ref nextInd

scroll :: Client -> Params -> IO ()
scroll client ps = do
  hSetBuffering stdout LineBuffering
  maybeSong <- getSong client
  songRef <- newIORef maybeSong
  -- Spawn a listener in a separate thread that
  -- listens for DBus `PropertiesChanged` signal.
  -- The callback updates the value in `songRef`.
  void $ setUpListener client songRef
  printSong ps songRef 0

playpause :: Client -> IO ()
playpause client = do
  status <- getStatus client
  -- `handleStatus` returns Maybe (IO ()), and sequnence_
  -- executes the action if the result is not Nothing.
  sequence_ $ status >>= handleStatus
  callNoReply client (callSpotify "PlayPause")

main :: IO ()
main = do
  client <- connectSession
  opts <- getSpotOnOptions
  case opts of
    Options Scroll ps -> scroll client ps
    Options Previous _ -> callNoReply client (callSpotify "Previous")
    Options PlayPause _ -> playpause client
    Options Next _ -> callNoReply client (callSpotify "Next")
