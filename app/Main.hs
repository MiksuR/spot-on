{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Int
import Data.IORef
import Data.Maybe (fromMaybe)
import DBus.Client
import System.IO

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as TIO

import SpotOnOptions
import SpotifyCommunications

-- Infinitely looping function that prints the song name in
-- the terminal while scrolling it.
printSong :: Params -> IORef (Maybe L.Text) -> Int64 -> IO ()
printSong ps ref ind = do
  maybeText <- readIORef ref
  let formatted = do
        text <- maybeText
        if L.length text >= textLength ps
        then return $ L.take (textLength ps) . L.drop ind $ L.cycle text
        else return $ L.justifyLeft (textLength ps) ' ' text
  -- TODO: Pack to Text earlier in the execution flow
  TIO.putStrLn . normalize NFC . T.pack $ formatted
  threadDelay . (1000*) . speed $ ps
  let overflow = and $ (ind+1 >=) . L.length <$> maybeText
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
