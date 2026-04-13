module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, sH)
import Types (GS)
import GameState (initGS, step, handleEv)
import Rendering (draw)

win :: Display
win = InWindow "Super Mario Bros — World 1-1" (sW, sH) (80,80)

bgCol :: Color
bgCol = makeColorI 92 148 252 255

main :: IO ()
main = play win bgCol 60 initGS draw handleEv step