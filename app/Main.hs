module Main where

import Data.IORef
import Control.Exception (catch)
import System.Exit (ExitCode(..))
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Constants (sW, sH)
import Types (GS, MS(MDead), Phase(LevelComplete), gMario, mState, gPhase, gLevels, gLevelIdx, lWorld, lNumber)
import GameState (initGS, step, handleEv)
import Rendering (Sprites, loadSprites, draw)
import Music

win :: Display
win = InWindow "Super Mario Bros -- World 1-1" (sW, sH) (80, 80)

bgCol :: Color
bgCol = makeColorI 92 148 252 255

desiredTrack :: GS -> TrackId
desiredTrack gs =
  let lvl  = gLevels gs !! gLevelIdx gs
      dead = mState (gMario gs) == MDead
      lc   = gPhase gs == LevelComplete
  in selectTrack dead lc (lWorld lvl) (lNumber lvl)

handleEvIO :: IORef MusicState -> Event -> GS -> IO GS
handleEvIO musicRef ev gs = do
  let gs' = handleEv ev gs
  switchIfNeeded musicRef (desiredTrack gs')
  return gs'

stepIO :: IORef MusicState -> Float -> GS -> IO GS
stepIO musicRef dt gs = do
  let gs' = step dt gs
  switchIfNeeded musicRef (desiredTrack gs')
  return gs'

main :: IO ()
main = do
  musicRef <- newIORef initMusicState
  sprites  <- loadSprites
  switchIfNeeded musicRef TOverworld
  playIO
      win
      bgCol
      60
      initGS
      (draw sprites)
      (handleEvIO musicRef)
      (stepIO musicRef)
    `catch` (\e -> do
      stopMusic musicRef
      case (e :: ExitCode) of
        ExitSuccess -> return ()
        exitCode    -> ioError (userError (show exitCode)))