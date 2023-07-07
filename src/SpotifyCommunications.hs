module SpotifyCommunications (
  callSpotify,
  getSong,
  getStatus,
  setUpListener
  ) where

import Control.Monad (void)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (traverse_)
import Data.IORef
import Data.List.Extra ((!?))
import DBus
import DBus.Client

import qualified Data.Map.Strict as M

callSpotify :: String -> MethodCall
callSpotify member =
  (methodCall
   (objectPath_ "/org/mpris/MediaPlayer2")
   (interfaceName_ "org.mpris.MediaPlayer2.Player")
   (memberName_ member))
  {methodCallDestination =
   Just $ busName_ "org.mpris.MediaPlayer2.spotify"}

extractSongFromMData :: Maybe Variant -> Maybe String
extractSongFromMData mData = do
  mDataVariant <- mData
  dict <- fromVariant mDataVariant :: Maybe (M.Map String Variant)
  artistsVariant <- M.lookup "xesam:albumArtist" dict
  titleVariant <- M.lookup "xesam:title" dict
  artists <- fromVariant artistsVariant :: Maybe [String]
  title <- fromVariant titleVariant :: Maybe String
  artist <- artists !? 0
  return $ title ++ " - " ++ artist ++ " - "

getSongFromSignal :: Signal -> Maybe String
getSongFromSignal sig = do
  body <- signalBody sig !? 1
  outerDict <- fromVariant body :: Maybe (M.Map String Variant)
  let mData = M.lookup "Metadata" outerDict
  extractSongFromMData mData

getSong :: Client -> IO (Maybe String)
getSong client = do
  songMData <- getProperty client (callSpotify "Metadata")
  return . extractSongFromMData $ eitherToMaybe songMData

getStatus :: Client -> IO (Maybe String)
getStatus client = do
  status <- getProperty client (callSpotify "PlaybackStatus")
  let statusString = do
        statusVariant <- eitherToMaybe status
        fromVariant statusVariant :: Maybe String
  return statusString

setUpListener :: Client -> IORef String -> IO ()
setUpListener client ref = do
  let match = matchAny {
    matchPath = Just $ objectPath_ "/org/mpris/MediaPlayer2",
    matchMember = Just $ memberName_ "PropertiesChanged"
  }
  -- I use the `traverse_` function here, because `getSongFromSignal`
  -- returns a value of type `Maybe String`. If the function returns `Nothing`,
  -- we don't want the program to do anything.
  void . addMatch client match $ traverse_ (writeIORef ref) . getSongFromSignal
