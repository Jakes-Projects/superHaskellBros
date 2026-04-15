module Rendering (draw) where

import Graphics.Gloss
import Constants (ts, sW, sH)
import Types

skyBlue, groundBrown, groundTop, brickRed, brickDark :: Color
skyBlue    = makeColorI 97  133 248 255
groundBrown= makeColorI 172 107  56 255
groundTop  = makeColorI 0   168   0 255
brickRed   = makeColorI 188  80  24 255
brickDark  = makeColorI 120  48  12 255

draw :: GS -> Picture
draw gs = pictures
  [ drawSky
  , translate (-(gCam gs)) 0 world
  , drawHUD gs
  , drawOverlay gs
  ]
  where
    world = pictures
      [ drawDecorations
      , drawTiles (gTiles gs)
      , drawCoins (gCoins gs)
      , drawPups  (gPups  gs)
      , drawEnem  (gEnem  gs)
      , drawMario (gMario gs)
      ]

drawSky :: Picture
drawSky = color skyBlue (rectangleSolid (fromIntegral sW) (fromIntegral sH))

ellipseS :: Float -> Float -> Picture
ellipseS rx ry = scale rx ry (circleSolid 1)

cloudPositions :: [(Float, Float, Int)]
cloudPositions =
  [ (6,10,1), (24,10,2), (44,10,1), (64,10,2)
  , (80,10,1), (98,10,2), (118,10,1), (136,10,1)
  , (154,10,2), (172,10,1), (192,10,1)
  ]

hillPositions :: [(Float, Float)]
hillPositions = [(0,1),(16,1),(48,1),(80,1),(96,1),(128,1),(160,1),(192,1)]

bushPositions :: [(Float, Float)]
bushPositions =
  [ (11,1),(23,1),(35,1),(57,1),(73,1)
  , (89,1),(105,1),(121,1),(145,1),(170,1),(185,1)
  ]

drawDecorations :: Picture
drawDecorations = pictures $
  map drawCloud cloudPositions ++
  map drawHill  hillPositions  ++
  map drawBush  bushPositions

drawCloud :: (Float,Float,Int) -> Picture
drawCloud (c,_r,sz) =
  let x = c * ts + ts/2
      y = 10 * ts
      w = fromIntegral sz * ts * 1.6
      h = fromIntegral sz * ts * 0.8
  in translate x y $ pictures
       [ color white (rectangleSolid w (h*0.55))
       , color white (translate (-w*0.2) (h*0.2) (rectangleSolid (w*0.5) (h*0.5)))
       , color white (translate (w*0.1)  (h*0.25)(rectangleSolid (w*0.45) (h*0.55)))
       , color (makeColorI 200 200 200 255)
               (translate 0 (-h*0.1) (rectangleSolid (w*0.9) (h*0.15)))
       ]

drawHill :: (Float,Float) -> Picture
drawHill (c,_) =
  let x = c * ts + ts
      y = ts
      r = ts * 2.8
      semi n rad = polygon [(rad * cos a, rad * sin a) | i <- [0..n :: Int],
                             let a = pi * fromIntegral i / fromIntegral n]
  in translate x y $ pictures
       [ color (makeColorI 0 128 0 255) (semi 16 r)
       , color (makeColorI 0 160 0 255) (semi 12 (r*0.6))
       , color (makeColorI 0 100 0 255) $ pictures
           [ translate (-r*0.35) (r*0.4) (circleSolid 3)
           , translate (r*0.35)  (r*0.4) (circleSolid 3)
           , translate 0         (r*0.62) (circleSolid 3)
           ]
       ]

drawBush :: (Float,Float) -> Picture
drawBush (c,_) =
  let x = c * ts
      y = ts * 0.5
  in translate x y $ pictures
       [ color (makeColorI 0 152 0 255) (ellipseS (ts*1.4) (ts*0.7))
       , color (makeColorI 0 180 0 255) (translate (-ts*0.3) (ts*0.1) (ellipseS (ts*0.9) (ts*0.6)))
       , color (makeColorI 0 180 0 255) (translate (ts*0.4)  (ts*0.1) (ellipseS (ts*0.8) (ts*0.55)))
       ]

drawTiles :: [Tile] -> Picture
drawTiles ts_ = pictures (map drawTile (filter visRow ts_))
  where visRow t = tRow t >= 0

drawTile :: Tile -> Picture
drawTile t = translate tx ty pic
  where
    tx  = fromIntegral (tCol t)*ts + ts/2
    ty  = fromIntegral (tRow t)*ts + ts/2
    pic = case tType t of
      Ground   -> drawGround
      Brick    -> drawBrick
      QBlock   -> drawQBlock
      Used     -> drawUsed
      PipeTop  -> drawPipeTop
      Pipe     -> drawPipe
      PipeR    -> blank
      FlagPole -> drawFlagPole
      FlagBase -> drawFlagBase
      Castle   -> drawCastle t
      SlopeLeft  -> drawGround
      SlopeRight -> drawGround

drawGround :: Picture
drawGround = pictures
  [ color groundBrown (rectangleSolid ts ts)
  , color (makeColorI 200 120 60 255) $ pictures
      [ translate 0 (ts*0.25) (rectangleSolid (ts*0.85) 2)
      , translate (ts*0.3) 0  (rectangleSolid 2 (ts*0.5))
      , translate (-ts*0.1) (-ts*0.15) (rectangleSolid 2 (ts*0.4))
      ]
  , color groundTop (translate 0 (ts/2 - 3) (rectangleSolid ts 6))
  , color (makeColorI 0 200 0 255) (translate 0 (ts/2 - 1) (rectangleSolid ts 2))
  ]

drawBrick :: Picture
drawBrick = pictures
  [ color brickRed (rectangleSolid ts ts)
  , color brickDark $ pictures
      [ translate 0       (ts*0.12)  (rectangleSolid ts 2)
      , translate 0       (-ts*0.12) (rectangleSolid ts 2)
      , translate (ts*0.25) 0 (rectangleSolid 2 ts)
      , translate (-ts*0.25) (ts*0.25) (rectangleSolid 2 (ts*0.5))
      ]
  ]

drawQBlock :: Picture
drawQBlock = pictures
  [ color (makeColorI 224 156 0 255) (rectangleSolid ts ts)
  , color (makeColorI 255 200 40 255) (translate (-1) 1 (rectangleSolid (ts-4) (ts-4)))
  , color brickDark $ pictures
      [ line [(-ts/2,-ts/2),(ts/2,-ts/2)]
      , line [(ts/2,-ts/2),(ts/2,ts/2)]
      , line [(ts/2,ts/2),(-ts/2,ts/2)]
      , line [(-ts/2,ts/2),(-ts/2,-ts/2)]
      ]
  , color (makeColorI 80 48 0 255) (translate (-2) 2 (scale 0.18 0.18 (text "?")))
  ]

drawUsed :: Picture
drawUsed = pictures
  [ color (makeColorI 140 96 48 255) (rectangleSolid ts ts)
  , color (makeColorI 100 64 24 255) $ pictures
      [ translate 0 (ts*0.12) (rectangleSolid ts 2)
      , translate 0 (-ts*0.12) (rectangleSolid ts 2)
      ]
  ]

drawPipeTop :: Picture
drawPipeTop = pictures
  [ color (makeColorI 0 168 0 255) (translate (ts/2) 0 (rectangleSolid (ts*2+4) ts))
  , color (makeColorI 0 210 0 255) (translate (ts/2) 0 (rectangleSolid (ts*1.6) (ts*0.7)))
  , color (makeColorI 0 130 0 255) (translate (ts*1.1) 0 (rectangleSolid (ts*0.35) ts))
  ]

drawPipe :: Picture
drawPipe = pictures
  [ color (makeColorI 0 152 0 255) (translate (ts/2) 0 (rectangleSolid (ts*2) ts))
  , color (makeColorI 0 196 0 255) (translate (ts*0.1) 0 (rectangleSolid (ts*0.45) ts))
  , color (makeColorI 0 120 0 255) (translate (ts*1.0) 0 (rectangleSolid (ts*0.3) ts))
  ]

drawFlagPole :: Picture
drawFlagPole = pictures
  [ color (makeColorI 188 188 188 255) (rectangleSolid 4 ts)
  , color (makeColorI 0 200 0 255)
      (translate 6 (ts*0.3) (polygon [(-2,-8),(-2,8),(12,0)]))
  ]

drawFlagBase :: Picture
drawFlagBase = pictures
  [ color (makeColorI 188 188 188 255) (rectangleSolid 4 ts)
  , color (makeColorI 140 140 140 255) (translate 0 (-ts/2+4) (rectangleSolid ts 8))
  ]

drawCastle :: Tile -> Picture
drawCastle t =
  let isBattlement = tRow t == 5
      isDoor = (tCol t == 208 || tCol t == 209) && tRow t <= 1
  in if isDoor then blank
     else pictures
       [ color (makeColorI 160 72 32 255) (rectangleSolid ts ts)
       , color (makeColorI 130 52 16 255) $ pictures
           [ translate 0 (ts*0.25) (rectangleSolid ts 2)
           , translate (ts*0.3) 0 (rectangleSolid 2 ts)
           ]
       , if isBattlement
           then color (makeColorI 100 36 8 255) (translate 0 (ts/2-3) (rectangleSolid (ts*0.5) 6))
           else blank
       ]

marioPixels :: [[Int]]
marioPixels =
  [ [0,0,0,1,1,1,1,1,0,0,0,0]
  , [0,0,1,1,1,1,1,1,1,1,0,0]
  , [0,0,3,3,3,2,2,3,2,0,0,0]
  , [0,3,2,3,2,2,2,3,2,2,2,0]
  , [0,3,2,3,3,2,2,2,3,2,2,2]
  , [0,3,3,2,2,2,2,3,3,3,3,0]
  , [0,0,0,2,2,2,2,2,2,2,0,0]
  , [0,0,5,5,1,5,5,5,1,5,0,0]
  , [0,5,5,5,1,5,5,5,1,5,5,0]
  , [5,5,5,5,1,1,1,1,1,5,5,5]
  , [2,2,5,5,5,5,5,5,5,5,2,2]
  , [2,2,2,5,5,5,5,5,5,2,2,2]
  , [0,0,2,2,2,0,0,2,2,2,0,0]
  , [0,3,3,3,0,0,0,0,3,3,3,0]
  , [3,3,3,3,0,0,0,0,3,3,3,3]
  , [3,3,3,0,0,0,0,0,0,3,3,3]
  ]

pixelColor :: Int -> Maybe Color
pixelColor 1 = Just (makeColorI 210 40  20  255)
pixelColor 2 = Just (makeColorI 240 185 120 255)
pixelColor 3 = Just (makeColorI 100 60  10  255)
pixelColor 4 = Just (makeColorI 60  30  0   255)
pixelColor 5 = Just (makeColorI 200 70  20  255)
pixelColor _ = Nothing

drawSpriteAt :: Float -> Float -> Float -> Int -> Picture
drawSpriteAt px py psz face =
  let rows   = length marioPixels
      cols   = length (head marioPixels)
      offX   = -fromIntegral cols * psz / 2
      offY   = -fromIntegral rows * psz / 2
      pixels = [ (ci, ri, v)
               | (ri, row) <- zip [0..] (reverse marioPixels)
               , (ci, v)   <- zip [0..] row ]
      drawPx (c, r, v) = case pixelColor v of
        Nothing -> blank
        Just col ->
          let x = offX + (fromIntegral c + 0.5) * psz
              y = offY + (fromIntegral r + 0.5) * psz
          in color col (translate x y (rectangleSolid psz psz))
  in translate px py $ scale (fromIntegral face) 1 $ pictures (map drawPx pixels)

drawMario :: Mario -> Picture
drawMario m
  | mState m == MDead =
      translate (mX m) (mY m) $
        color (makeColorI 210 40 20 255) (circleSolid 14)
  | blink = blank
  | otherwise = drawSpriteAt (mX m) (mY m) pxSize (mFace m)
  where
    blink  = mInv m > 0 && even (floor (mInv m * 10) :: Int)
    pxSize = if mState m == Big then 4.5 else 2.5

drawEnem :: [Enemy] -> Picture
drawEnem = pictures . map drawE

drawE :: Enemy -> Picture
drawE e = case eState e of
  EDead _ -> translate cx (eY e + 5) flat
  _       -> if shouldDrawAlive e
               then translate cx (eY e + ts/2) (drawEnemyBody e)
               else blank
  where
    cx   = eX e + ts/2
    flat = color (makeColorI 130 70 15 255) (rectangleSolid (ts*0.9) 9)

    shouldDrawAlive e = case eState e of
      EAlive        -> True
      EShell _ _    -> True
      EPiranha _ up -> up
      _             -> False

    drawEnemyBody e = case eType e of
      Goomba  -> drawGoomba
      Koopa   -> drawKoopa e
      Piranha -> translate 0 (ts*0.6) drawPiranha

drawGoomba :: Picture
drawGoomba = pictures
  [ color (makeColorI 130 70 15 255)  (rectangleSolid (ts*0.85) (ts*0.8))
  , color (makeColorI 70 35 0 255)    (translate 0 (-ts*0.28) (rectangleSolid (ts*0.9) (ts*0.22)))
  , color white  (translate (-7) 5 (circleSolid 5))
  , color white  (translate  (7) 5 (circleSolid 5))
  , color black  (translate (-7) 5 (circleSolid 2.5))
  , color black  (translate  (7) 5 (circleSolid 2.5))
  ]

drawKoopa :: Enemy -> Picture
drawKoopa e = case eState e of
  EShell t True  -> pictures [ color (makeColorI 255 0 0 255) (rectangleSolid (ts*1.2) (ts*1.2))
                             , translate 0 0 $ color white $ scale 0.3 0.3 $ text (show (round t)) ]
  EShell t False -> pictures [ color (makeColorI 0 0 255 255) (rectangleSolid ts ts)
                             , translate 0 0 $ color white $ scale 0.3 0.3 $ text (show (round t)) ]
  _              -> drawLiveKoopa

drawLiveKoopa :: Picture
drawLiveKoopa = pictures
  [ color (makeColorI 0 175 0 255)    (rectangleSolid (ts*0.8) (ts*1.05))
  , color (makeColorI 255 215 90 255) (translate 0 (ts*0.32) (circleSolid (ts*0.28)))
  , color (makeColorI 0 120 0 255)    (scale (ts*0.32) (ts*0.48) (circleSolid 1))
  , color white  (translate (-6) (ts*0.28) (circleSolid 4))
  , color white  (translate  (6) (ts*0.28) (circleSolid 4))
  , color black  (translate (-6) (ts*0.28) (circleSolid 2))
  , color black  (translate  (6) (ts*0.28) (circleSolid 2))
  ]

drawPiranha :: Picture
drawPiranha = pictures
  [ color (makeColorI 0 180 0 255) (circleSolid (ts*0.45))
  , color (makeColorI 220 50 50 255) (translate 0 (ts*0.2) (circleSolid (ts*0.2)))
  , color white (translate (-6) 6 (circleSolid 4))
  , color white (translate  (6) 6 (circleSolid 4))
  , color black (translate (-6) 6 (circleSolid 2))
  , color black (translate  (6) 6 (circleSolid 2))
  , color (makeColorI 0 150 0 255) (translate 0 (-ts*0.3) (rectangleSolid (ts*0.25) (ts*0.5)))
  , color (makeColorI 0 200 0 255) (translate (-ts*0.3) (-ts*0.1) (ellipseS (ts*0.2) (ts*0.15)))
  , color (makeColorI 0 200 0 255) (translate (ts*0.3) (-ts*0.1) (ellipseS (ts*0.2) (ts*0.15)))
  ]

drawPups :: [PUp] -> Picture
drawPups = pictures . map drawPup

drawPup :: PUp -> Picture
drawPup p
  | not (pAlive p) = blank
  | otherwise = translate (pX p + ts/2) (pY p) $
      pictures [ color (makeColorI 220 50 50 255) (rectangleSolid (ts*0.85) (ts*0.85))
               , color white (translate (-4) 2 (scale 0.22 0.22 (text "M"))) ]

drawCoins :: [(Float,Float,Bool)] -> Picture
drawCoins = pictures . map drawCoin

drawCoin :: (Float,Float,Bool) -> Picture
drawCoin (_,_,True) = blank
drawCoin (x,y,_)    = translate x y $
  pictures [ color (makeColorI 255 215 0 255) (circleSolid 9)
           , color (makeColorI 255 240 60 255) (circleSolid 5.5) ]

drawHUD :: GS -> Picture
drawHUD gs = translate (-370) 262 $ pictures
  [ color white (scale 0.14 0.14 (text ("SCORE " ++ show (gScore gs))))
  , translate 0 (-22) $ color white (scale 0.14 0.14 (text ("LIVES  " ++ show (gLives gs))))
  , translate 250 0   $ color white (scale 0.14 0.14 (text "WORLD 1-1"))
  ]

drawOverlay :: GS -> Picture
drawOverlay gs = case gPhase gs of
  Play -> blank
  Over -> mkOv (dark red)                    "GAME OVER"  ("Lives: " ++ show (gLives gs))
  Win  -> mkOv (makeColorI 255 215 0 255)    "YOU WIN!"   ("Score: " ++ show (gScore gs))
  where
    mkOv c t1 t2 = pictures
      [ color (withAlpha 0.65 black) (rectangleSolid 900 700)
      , color c     (translate (-155) 40   (scale 0.45 0.45 (text t1)))
      , color white (translate (-160) (-25) (scale 0.2  0.2  (text t2)))
      , color white (translate (-110) (-65) (scale 0.16 0.16 (text "Press R to restart")))
      ]