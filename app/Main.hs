module Main where

import Data.IORef
import Control.Exception (catch)
import System.Exit (ExitCode(..))
import System.Process (ProcessHandle)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Constants (sW, sH)
import Types (GS, MS(MDead), Phase(LevelComplete, LevelIntro, PipeEntry), TType(PipeTop), gMario, mState, gPhase, gLevels, gLevelIdx, lWorld, lNumber, gTiles, tType, tCol, gFlagTimer, gPipeTimer)
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
  | gPhase gs == LevelIntro =
      -- For the 1-2 pipe-entry intro level (lNumber == 0), start the overworld
      -- theme on the black screen so it's already playing when Mario appears.
      -- All other levels stay silent during the intro screen.
      let lvl = gLevels gs !! gLevelIdx gs
      in if lNumber lvl == 0 then TOverworld else TSilent
  | gPhase gs == PipeEntry  =
      -- Play overworld music during the walk phase; silence once Mario starts sinking.
      let startX    = gFlagTimer gs
          pipeCX    = case [ fromIntegral (tCol t) * 32 + 16
                           | t <- gTiles gs, tType t == PipeTop ] of
                        (x:_) -> x
                        []    -> startX + 32 * 8
          walkEnd   = max 0 (pipeCX - startX) / 80
      in if gPipeTimer gs < walkEnd then TOverworld else TSilent
  | otherwise =
      let lvl  = gLevels gs !! gLevelIdx gs
          dead = mState (gMario gs) == MDead
          lc   = gPhase gs == LevelComplete
      in selectTrack dead lc (lWorld lvl) (lNumber lvl)

handleEvIO :: IORef MusicState -> IORef (Maybe ProcessHandle) -> Event -> GS -> IO GS
handleEvIO musicRef flagpoleRef ev gs = do
  let gs' = handleEv ev gs
  case ev of
    -- Level-select keys: always restart the track from the beginning,
    -- even if the new level shares the same theme as the old one.
    EventKey (Char d) Down _ _ | d >= '1' && d <= '8' ->
      restartTrack musicRef (desiredTrack gs')
    EventKey _ Down _ _ -> do
      switchIfNotBlocked musicRef flagpoleRef (desiredTrack gs')
      case jumpSfx (gMario gs) (gMario gs') of
        Just sfx -> playSfx sfx
        Nothing  -> return ()
    _ -> switchIfNotBlocked musicRef flagpoleRef (desiredTrack gs')
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
