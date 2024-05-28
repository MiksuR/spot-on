module Main (main) where

import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Maybe (fromMaybe)
import DBus.Client
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Normalize

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
  -- TODO: Pack to Text earlier in the execution flow
  TIO.putStrLn . normalize NFC . T.pack $ formatted
  threadDelay . (1000*) . speed $ ps
  let overflow = and $ (ind+1 >=) . length <$> maybeText
  printSong ps ref $ if overflow then 0 else ind+1

scroll :: Client -> Params -> IO ()
scroll client ps = do
  hSetBuffering stdout LineBuffering
  songRef <- getSong client >>= newIORef
  -- Spawn a listener in a separate thread that
  -- listens for DBus `PropertiesChanged` signal.
  -- The callback updates the value in `songRef`.
  _ <- setUpListener client songRef
  printSong ps songRef 0

playpause :: Client -> IO ()
playpause client = do
  -- handleStatus sends a corresponding message to Polybar
  getStatus client >>= mapM_ handleStatus
  callNoReply client (spotifyCall "PlayPause")

main :: IO ()
main = do
  client <- connectSession
  opts <- getSpotOnOptions
  case opts of
    Options Scroll ps -> scroll client ps
    Options Previous _ -> callNoReply client (spotifyCall "Previous")
    Options PlayPause _ -> playpause client
    Options Next _ -> callNoReply client (spotifyCall "Next")
