module Main where

import Data.IORef
import Control.Exception (catch)
import System.Exit (ExitCode(..))
import System.Process (ProcessHandle)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Constants (sW, sH)
import Types (GS, MS(MDead), Phase(LevelComplete, LevelIntro), gMario, mState, gPhase, gLevels, gLevelIdx, lWorld, lNumber)
import GameState (initGS, step, handleEv)
import Rendering (Sprites, loadSprites, draw)
import Music
import Sound (detectSoundEvents, jumpSfx, playSfx, playSfxHandle, SoundEvent(..))

win :: Display
win = InWindow "Super Mario Bros -- World 1-1" (sW, sH) (80, 80)

bgCol :: Color
bgCol = makeColorI 92 148 252 255

desiredTrack :: GS -> TrackId
desiredTrack gs
  | gPhase gs == LevelIntro = TSilent
  | otherwise =
      let lvl  = gLevels gs !! gLevelIdx gs
          dead = mState (gMario gs) == MDead
          lc   = gPhase gs == LevelComplete
      in selectTrack dead lc (lWorld lvl) (lNumber lvl)

handleEvIO :: IORef MusicState -> IORef (Maybe ProcessHandle) -> Event -> GS -> IO GS
handleEvIO musicRef flagpoleRef ev gs = do
  let gs' = handleEv ev gs
  switchIfNotBlocked musicRef flagpoleRef (desiredTrack gs')
  case ev of
    EventKey _ Down _ _ ->
      case jumpSfx (gMario gs) (gMario gs') of
        Just sfx -> playSfx sfx
        Nothing  -> return ()
    _ -> return ()
  return gs'

stepIO :: IORef MusicState -> IORef (Maybe ProcessHandle) -> Float -> GS -> IO GS
stepIO musicRef flagpoleRef dt gs = do
  let gs'    = step dt gs
      events = detectSoundEvents gs gs'
  -- Play the flagpole SFX and capture its handle to block the music switch.
  -- All other SFX are fire-and-forget.
  mapM_ (\ev -> case ev of
    SfxFlagpole -> do
      mh <- playSfxHandle SfxFlagpole
      writeIORef flagpoleRef mh
    _ -> playSfx ev
    ) events
  switchIfNotBlocked musicRef flagpoleRef (desiredTrack gs')
  return gs'

main :: IO ()
main = do
  musicRef    <- newIORef initMusicState
  flagpoleRef <- newIORef (Nothing :: Maybe ProcessHandle)
  sprites     <- loadSprites
  playIO
      win
      bgCol
      60
      initGS
      (draw sprites)
      (handleEvIO musicRef flagpoleRef)
      (stepIO musicRef flagpoleRef)
    `catch` (\e -> do
      stopMusic musicRef
      case (e :: ExitCode) of
        ExitSuccess -> return ()
        exitCode    -> ioError (userError (show exitCode)))
