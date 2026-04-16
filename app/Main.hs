module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game   -- switched from Pure.Game
import Constants (sW, sH)
import Types (GS)
import GameState (initGS, step, handleEv)
import Rendering (Sprites, loadSprites, draw)

win :: Display
win = InWindow "Super Mario Bros — World 1-1" (sW, sH) (80, 80)

bgCol :: Color
bgCol = makeColorI 92 148 252 255

main :: IO ()
main = do
  sprites <- loadSprites                    -- load all PNGs once at startup
  playIO
    win
    bgCol
    60
    initGS
    (draw sprites)                          -- GS -> IO Picture
    (\ev gs -> return $ handleEv ev gs)     -- event handler wrapped in IO
    (\dt gs -> return $ step dt gs)         -- step wrapped in IO
