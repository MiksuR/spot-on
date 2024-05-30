module SpotifyCommunications (
    spotifyCall,
    getSong,
    getStatus,
    handleStatus,
    setUpListener
  ) where

import Control.Monad
import Data.Either.Extra (eitherToMaybe)
import Data.IORef
import Data.List.Extra ((!?))
import Data.Maybe (isNothing)
import DBus
import DBus.Client
import System.Process (callCommand)

import qualified Data.Map.Strict as M

spotifyCall :: String -> MethodCall
spotifyCall member =
  (methodCall
   (objectPath_ "/org/mpris/MediaPlayer2")
   (interfaceName_ "org.mpris.MediaPlayer2.Player")
   (memberName_ member))
  {methodCallDestination =
   Just $ busName_ "org.mpris.MediaPlayer2.spotify"}

extractSong :: Variant -> Maybe String
extractSong mData = do
  dict <- fromVariant mData :: Maybe (M.Map String Variant)
  artistsVariant <- M.lookup "xesam:albumArtist" dict
  titleVariant <- M.lookup "xesam:title" dict
  artists <- fromVariant artistsVariant :: Maybe [String]
  title <- fromVariant titleVariant :: Maybe String
  artist <- artists !? 0
  return $ title ++ " - " ++ artist ++ " - "

getSong :: Client -> IO (Maybe String)
getSong client = do
  songMData <- getProperty client (spotifyCall "Metadata")
  return $ extractSong =<< eitherToMaybe songMData

getStatus :: Client -> IO (Maybe String)
getStatus client = do
  status <- getProperty client (spotifyCall "PlaybackStatus")
  let statusString = do
        statusVariant <- eitherToMaybe status
        fromVariant statusVariant :: Maybe String
  return statusString

handleStatus :: String -> IO ()
handleStatus status =
  case status of
    "Playing" -> callCommand "polybar-msg action \"#spot-on-playpause.hook.1\" > /dev/null"
    "Paused" -> callCommand "polybar-msg action \"#spot-on-playpause.hook.0\" > /dev/null"
    _ -> return ()

callback :: Client -> IORef (Maybe String) -> Signal -> IO ()
callback c ref sig = do
  let body = signalBody sig !? 1
  let outerDict = body >>= fromVariant :: Maybe (M.Map String Variant)

  -- Handle metadata update
  let maybeMetadataV = outerDict >>= M.lookup "Metadata"
  mapM_ (writeIORef ref . extractSong) maybeMetadataV

  -- Handle status update
  let maybeStatusV = outerDict >>= M.lookup "PlaybackStatus"
  let maybeStatus = maybeStatusV >>= fromVariant :: Maybe String
  mapM_ handleStatus maybeStatus

  -- Try updating Nothing value
  song <- readIORef ref
  when (isNothing song) $ getSong c >>= writeIORef ref

setUpListener :: Client -> IORef (Maybe String) -> IO ()
setUpListener client = void . addMatch client match . callback client
  where
    match = matchAny {
      matchPath = Just $ objectPath_ "/org/mpris/MediaPlayer2",
      matchMember = Just $ memberName_ "PropertiesChanged"
    }
