module Music
  ( TrackId(..)
  , MusicState
  , initMusicState
  , selectTrack
  , switchIfNeeded
  , stopMusic
  ) where

import Data.IORef
import System.Process (ProcessHandle, spawnProcess, terminateProcess)
import System.IO.Error (catchIOError)

data TrackId
  = TOverworld
  | TUnderground
  | TUnderwater
  | TCastle
  | TLostALife
  | TLevelComplete
  | TSilent
  deriving (Eq, Show)

trackPath :: TrackId -> Maybe FilePath
trackPath TOverworld     = Just "assets/overworld_theme.mp3"
trackPath TUnderground   = Just "assets/underground_theme.mp3"
trackPath TUnderwater    = Just "assets/underwater_theme.mp3"
trackPath TCastle        = Just "assets/castle_theme.mp3"
trackPath TLostALife     = Just "assets/lost_a_life.mp3"
trackPath TLevelComplete = Just "assets/level_complete.mp3"
trackPath TSilent        = Nothing

isLooping :: TrackId -> Bool
isLooping TLostALife     = False
isLooping TLevelComplete = False
isLooping _              = True

selectTrack :: Bool -> Bool -> Int -> Int -> TrackId
selectTrack True _    _ _ = TLostALife
selectTrack _    True _ _ = TLevelComplete
selectTrack _    _    w n
  | (w == 1 && n == 4) || (w == 2 && n == 4) = TCastle
  | w == 2 && n == 2                          = TUnderwater
  | w == 1 && n == 2                          = TUnderground
  | otherwise                                 = TOverworld

data MusicState = MusicState
  { msCurrentTrack :: TrackId
  , msHandle       :: Maybe ProcessHandle
  }

initMusicState :: MusicState
initMusicState = MusicState { msCurrentTrack = TSilent, msHandle = Nothing }

stopCurrent :: MusicState -> IO ()
stopCurrent ms = case msHandle ms of
  Nothing -> return ()
  Just h  -> catchIOError (terminateProcess h) (\_ -> return ())

startTrack :: TrackId -> IO (Maybe ProcessHandle)
startTrack TSilent = return Nothing
startTrack tid     = case trackPath tid of
  Nothing -> return Nothing
  Just fp -> do
    let args = if isLooping tid then [fp, "-r", "0"] else [fp]
    h <- spawnProcess "afplay" args
    return (Just h)

switchIfNeeded :: IORef MusicState -> TrackId -> IO ()
switchIfNeeded ref wanted = do
  ms <- readIORef ref
  if wanted == msCurrentTrack ms
    then return ()
    else do
      stopCurrent ms
      h <- startTrack wanted
      writeIORef ref MusicState { msCurrentTrack = wanted, msHandle = h }

stopMusic :: IORef MusicState -> IO ()
stopMusic ref = do
  ms <- readIORef ref
  stopCurrent ms
  writeIORef ref initMusicState