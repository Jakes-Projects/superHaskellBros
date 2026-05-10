module Music
  ( TrackId(..)
  , MusicState
  , initMusicState
  , selectTrack
  , switchIfNeeded
  , switchIfNotBlocked
  , stopMusic
  ) where

import Data.IORef
import System.Process (ProcessHandle, spawnProcess, terminateProcess, getProcessExitCode)
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

-- | Switch to 'wanted' track immediately, unless it is already playing.
switchIfNeeded :: IORef MusicState -> TrackId -> IO ()
switchIfNeeded ref wanted = do
  ms <- readIORef ref
  if wanted == msCurrentTrack ms
    then return ()
    else do
      stopCurrent ms
      h <- startTrack wanted
      writeIORef ref MusicState { msCurrentTrack = wanted, msHandle = h }

-- | Like 'switchIfNeeded', but when the desired track is 'TLevelComplete'
--   the switch is held off until 'blockerRef' contains Nothing or a finished
--   process (i.e. the flagpole SFX has finished playing).
--   For all other tracks the blocker is ignored and the switch happens normally.
switchIfNotBlocked :: IORef MusicState -> IORef (Maybe ProcessHandle) -> TrackId -> IO ()
switchIfNotBlocked musicRef blockerRef wanted = case wanted of
  TLevelComplete -> do
    blocker <- readIORef blockerRef
    blocked <- case blocker of
      Nothing -> return False          -- no blocker set, allow immediately
      Just h  -> do
        code <- getProcessExitCode h   -- non-blocking poll
        case code of
          Nothing -> return True       -- still running, stay blocked
          Just _  -> do
            writeIORef blockerRef Nothing  -- process done, clear blocker
            return False
    if blocked
      then return ()                   -- music stays silent until flagpole finishes
      else switchIfNeeded musicRef wanted
  _ -> switchIfNeeded musicRef wanted  -- all other tracks switch normally

stopMusic :: IORef MusicState -> IO ()
stopMusic ref = do
  ms <- readIORef ref
  stopCurrent ms
  writeIORef ref initMusicState
