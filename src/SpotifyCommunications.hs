module SpotifyCommunications (
    callSpotify,
    getSong,
    getStatus,
    handleStatus,
    setUpListener
  ) where

import Control.Monad
import Data.Either.Extra (eitherToMaybe)
import Data.IORef
import Data.List.Extra ((!?))
import Data.Maybe (mapMaybe)
import DBus
import DBus.Client
import System.Process (callCommand)

import qualified Data.Map.Strict as M

callSpotify :: String -> MethodCall
callSpotify member =
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
  songMData <- getProperty client (callSpotify "Metadata")
  return $ extractSong =<< eitherToMaybe songMData

getStatus :: Client -> IO (Maybe String)
getStatus client = do
  status <- getProperty client (callSpotify "PlaybackStatus")
  let statusString = do
        statusVariant <- eitherToMaybe status
        fromVariant statusVariant :: Maybe String
  return statusString

handleStatus :: String -> Maybe (IO ())
handleStatus status =
  case status of
    "Playing" -> Just $ callCommand "polybar-msg action \"#spot-on-playpause.hook.0\""
    "Paused" -> Just $ callCommand "polybar-msg action \"#spot-on-playpause.hook.1\""
    _ -> Nothing

callback :: IORef String -> Signal -> IO ()
callback ref sig = do
  let body = signalBody sig !? 1
  let outerDict = body >>= fromVariant :: Maybe (M.Map String Variant)
  -- If the body contains metadata, then update IORef with
  -- new song title.
  -- If the body contains playback status, send messsage to polybar.
  -- If both lookups fail, do nothing.
  sequence_ $ mapMaybe (outerDict >>=)
    [ (return . writeIORef ref) <=< extractSong <=< M.lookup "Metadata",
      handleStatus <=< fromVariant <=< M.lookup "PlaybackStatus" ]

setUpListener :: Client -> IORef String -> IO ()
setUpListener client = void . addMatch client match . callback
  where
    match = matchAny {
      matchPath = Just $ objectPath_ "/org/mpris/MediaPlayer2",
      matchMember = Just $ memberName_ "PropertiesChanged"
    }
