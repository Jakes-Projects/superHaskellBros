module Rendering (Sprites, loadSprites, draw) where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Constants (ts, sW, sH)
import Types

-- ─── Sprite record ────────────────────────────────────────────────────────────
-- Only the sprites we actually have PNGs for.
-- Everything else (tiles, coins, decorations, HUD) keeps its existing
-- hand-drawn primitive rendering.

data Sprites = Sprites
  { -- Small Mario
    spMarioStand  :: Picture
  , spMarioRun1   :: Picture
  , spMarioRun2   :: Picture
  , spMarioRun3   :: Picture
  , spMarioJump   :: Picture
  , spMarioSkid   :: Picture
  , spMarioDeath  :: Picture
    -- Big Mario
  , spBigStand    :: Picture
  , spBigRun1     :: Picture
  , spBigRun2     :: Picture
  , spBigRun3     :: Picture
  , spBigJump     :: Picture
  , spBigSkid     :: Picture
  , spBigCrouch   :: Picture
    -- Goomba
  , spGoomba1       :: Picture
  , spGoomba2       :: Picture
  , spGoombaCrushed :: Picture
    -- Koopa
  , spKoopa1         :: Picture
  , spKoopa2         :: Picture
  , spKoopaShell     :: Picture
  , spKoopaResetting :: Picture
    -- Blocks
  , spBlockGround      :: Picture
  , spBlockBrick       :: Picture
  , spBlockBrickBroken :: Picture
  , spBlockHitEmpty    :: Picture
  , spBlockQuestion1   :: Picture
  , spBlockQuestion2   :: Picture
  , spBlockQuestion3   :: Picture
    -- Collectibles
  , spMushroom :: Picture
  , spCoin1    :: Picture
  , spCoin2    :: Picture
  , spCoin3    :: Picture
  , spCoin4    :: Picture
  }

-- ─── Loader ───────────────────────────────────────────────────────────────────

loadPNG :: FilePath -> IO Picture
loadPNG path = do
  result <- loadJuicyPNG path
  case result of
    Just pic -> return pic
    Nothing  -> do
      putStrLn $ "WARNING: could not load sprite: " ++ path
      return $ color magenta (rectangleSolid 32 32)  -- hot-pink placeholder

loadSprites :: IO Sprites
loadSprites = Sprites
  -- Small Mario
  <$> loadPNG "assets/mario_stand.png"
  <*> loadPNG "assets/mario_run_1.png"
  <*> loadPNG "assets/mario_run_2.png"
  <*> loadPNG "assets/mario_run_3.png"
  <*> loadPNG "assets/mario_jump.png"
  <*> loadPNG "assets/mario_skid.png"
  <*> loadPNG "assets/mario_death.png"
  -- Big Mario
  <*> loadPNG "assets/mario_big_stand.png"
  <*> loadPNG "assets/mario_big_run_1.png"
  <*> loadPNG "assets/mario_big_run_2.png"
  <*> loadPNG "assets/mario_big_run_3.png"
  <*> loadPNG "assets/mario_big_jump.png"
  <*> loadPNG "assets/mario_big_skid.png"
  <*> loadPNG "assets/mario_big_crouch.png"
  -- Goomba
  <*> loadPNG "assets/goomba_1.png"
  <*> loadPNG "assets/goomba_2.png"
  <*> loadPNG "assets/goomba_crushed.png"
  -- Koopa
  <*> loadPNG "assets/koopa_green_1.png"
  <*> loadPNG "assets/koopa_green_2.png"
  <*> loadPNG "assets/koopa_green_shell.png"
  <*> loadPNG "assets/koopa_green_resetting.png"
  -- Blocks
  <*> loadPNG "assets/block_ground.png"
  <*> loadPNG "assets/block_brick.png"
  <*> loadPNG "assets/block_brick_broken.png"
  <*> loadPNG "assets/block_hit_empty.png"
  <*> loadPNG "assets/block_question_1.png"
  <*> loadPNG "assets/block_question_2.png"
  <*> loadPNG "assets/block_question_3.png"
  -- Collectibles
  <*> loadPNG "assets/mushroom.png"
  <*> loadPNG "assets/coin_1.png"
  <*> loadPNG "assets/coin_2.png"
  <*> loadPNG "assets/coin_3.png"
  <*> loadPNG "assets/coin_4.png"

-- ─── World Y offset ──────────────────────────────────────────────────────────
-- Gloss Y=0 is the window centre. We shift the world down so that:
--   • Row 0 top  = -sH/2 + 3*ts = -204  (ground surface, Mario stands here)
--   • Row -2 bottom = -sH/2 = -300       (exactly the window bottom, no gap)
-- Three ground rows (0, -1, -2) fill the bottom 96 units of the window.
worldYOffset :: Float
worldYOffset = -(fromIntegral sH / 2) + 3.0 * ts - ts   -- = -236

-- ─── Top-level draw (IO because we loaded sprites in IO) ──────────────────────

draw :: Sprites -> GS -> IO Picture
draw spr gs = return $ pictures
  [ drawSky
  , translate (-(gCam gs)) worldYOffset world
  , drawHUD gs
  , drawOverlay gs
  ]
  where
    -- Use mAnim as a global clock for block/coin animation.
    -- It increments every frame at 60fps so it's a reliable ticker.
    clock = mAnim (gMario gs)
    world = pictures
      [ drawDecorations
      , drawTiles   spr clock (gTiles gs)
      , drawCoins   spr clock (gCoins gs)
      , drawPups    spr       (gPups  gs)
      , drawFirebars          (gFirebars gs)
      , drawEnem    spr clock (gEnem  gs)
      , drawMario   spr       (gMario gs)
      ]

-- ─── Mario ────────────────────────────────────────────────────────────────────

-- PNGs are at NES native resolution (16x16 small, 16x32 big).
-- gloss-juicy loads them at 1 PNG pixel = 1 game unit.
--
-- The correct display scale is game_w / nes_w = 800 / 256 = 3.125.
-- This makes Mario occupy the same fraction of the screen he did on a real
-- NES (16/256 = 6.25% of screen width), regardless of the tile size used
-- for physics. At this scale small Mario = 50x50 units, big = 50x100 units.
-- PNGs are pre-scaled to exactly 3x NES native resolution using integer
-- NEAREST resampling (48x48 small, 48x96 big), so Gloss never has to
-- scale them — no interpolation, no blur, perfectly crisp pixel art.
marioScale :: Float
marioScale = 1.0

-- mFace is 1 (right) or -1 (left).
-- For death we do NOT flip regardless of facing direction.
-- For everything else, scale x by mFace to mirror the sprite.

drawMario :: Sprites -> Mario -> Picture
drawMario spr m
  | mState m == MDead =
      translate (mX m) (mY m)
        $ scale marioScale marioScale
        $ spMarioDeath spr
  | blink = blank
  | otherwise =
      translate (mX m) (mY m)
        $ scale (marioScale * fromIntegral (mFace m)) marioScale
        $ pickMarioFrame spr m
  where
    blink = mInv m > 0 && even (floor (mInv m * 10) :: Int)

pickMarioFrame :: Sprites -> Mario -> Picture
pickMarioFrame spr m =
  let big      = mState m == Big
      airborne = not (mGround m)
      -- walk cycle: mAnim increments each frame; we bucket into 3 frames
      wFrame   = (floor (mAnim m * 10) :: Int) `mod` 3
      -- skidding: moving right but VX is negative, or moving left but VX positive
      skidding = (mFace m == 1 && mVX m < -10)
              || (mFace m == (-1) && mVX m > 10)
      -- standing still
      still    = abs (mVX m) < 5 && mGround m
  in if big
     then pickBigFrame  spr airborne skidding still wFrame
     else pickSmallFrame spr airborne skidding still wFrame

pickSmallFrame :: Sprites -> Bool -> Bool -> Bool -> Int -> Picture
pickSmallFrame spr airborne skidding still wFrame
  | airborne  = spMarioJump  spr
  | skidding  = spMarioSkid  spr
  | still     = spMarioStand spr
  | wFrame == 0 = spMarioRun1  spr
  | wFrame == 1 = spMarioRun2  spr
  | otherwise   = spMarioRun3  spr

pickBigFrame :: Sprites -> Bool -> Bool -> Bool -> Int -> Picture
pickBigFrame spr airborne skidding still wFrame
  | airborne  = spBigJump   spr
  | skidding  = spBigSkid   spr
  | still     = spBigStand  spr
  | wFrame == 0 = spBigRun1  spr
  | wFrame == 1 = spBigRun2  spr
  | otherwise   = spBigRun3  spr

-- ─── Enemies ──────────────────────────────────────────────────────────────────

drawEnem :: Sprites -> Float -> [Enemy] -> Picture
drawEnem spr clock = pictures . map (drawE spr clock)

drawE :: Sprites -> Float -> Enemy -> Picture
drawE spr clock e = case eState e of
  EDead _         -> translate cx (eY e + 5)          (spGoombaCrushed spr)
  EShell _ moving -> translate cx (eY e + spriteHalf) (shellPic moving)
  _               ->
    if shouldDrawAlive (eState e)
      then translate cx (eY e + spriteHalf) (drawEnemyBody spr clock e)
      else blank
  where
    cx = eX e + ts/2
    -- Enemy sprites are 48px tall (16 NES px * 3). Half = 24.
    -- Centering at eY+24 puts the sprite bottom exactly at eY,
    -- which is the physics floor — no clipping into the ground.
    spriteHalf = 24

    shellPic moving
      | moving    = spKoopaResetting spr
      | otherwise = spKoopaShell spr

    shouldDrawAlive st = case st of
      EAlive        -> True
      EPiranha _ up -> up
      _             -> False

drawEnemyBody :: Sprites -> Float -> Enemy -> Picture
drawEnemyBody spr clock e = case eType e of
  Goomba  -> scale marioScale marioScale $ goombaFrame spr clock
  Koopa   -> scale marioScale marioScale $ koopaFrame  spr clock e
  Piranha -> translate 0 (ts * 0.6) drawPiranha

-- Goomba walks with 2 frames, alternating at ~8fps using the global clock.
-- NES Goomba animates at roughly 8 frames/sec.
goombaFrame :: Sprites -> Float -> Picture
goombaFrame spr clock =
  if even (floor (clock * 8) :: Int)
    then spGoomba1 spr
    else spGoomba2 spr

-- Koopa walks with 2 frames at the same rate.
-- When in shell state the shell sprite is shown regardless of clock.
koopaFrame :: Sprites -> Float -> Enemy -> Picture
koopaFrame spr clock e = case eState e of
  EShell _ _ -> spKoopaShell spr
  _          ->
    if even (floor (clock * 8) :: Int)
      then spKoopa1 spr
      else spKoopa2 spr

-- ─── Primitives kept from original (no sprites available yet) ─────────────────

-- Sky
drawSky :: Picture
drawSky = color skyBlue (rectangleSolid (fromIntegral sW) (fromIntegral sH))

skyBlue :: Color
skyBlue = makeColorI 97 133 248 255

-- Decorations (clouds, hills, bushes) — no sprites, keep originals
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
       , color white (translate (w*0.1)  (h*0.25) (rectangleSolid (w*0.45) (h*0.55)))
       , color (makeColorI 200 200 200 255)
               (translate 0 (-h*0.1) (rectangleSolid (w*0.9) (h*0.15)))
       ]

drawHill :: (Float,Float) -> Picture
drawHill (c,_) =
  let x = c * ts + ts
      y = ts
      r = ts * 2.8
      semi n rad = polygon
        [(rad * cos a, rad * sin a) | i <- [0..n :: Int],
         let a = pi * fromIntegral i / fromIntegral n]
  in translate x y $ pictures
       [ color (makeColorI 0 128 0 255) (semi 16 r)
       , color (makeColorI 0 160 0 255) (semi 12 (r*0.6))
       , color (makeColorI 0 100 0 255) $ pictures
           [ translate (-r*0.35) (r*0.4)  (circleSolid 3)
           , translate (r*0.35)  (r*0.4)  (circleSolid 3)
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

-- ─── Tiles ────────────────────────────────────────────────────────────────────

-- Block sprites are 48x48 px (16 NES px * 3).
-- The physics grid cell is ts=32 game units.
-- Tiles must render at exactly 32x32 to avoid clipping neighbours.
-- Scale factor: ts / 48 = 32 / 48 = 0.6667
tileScale :: Float
tileScale = ts / 48

drawTiles :: Sprites -> Float -> [Tile] -> Picture
drawTiles spr clock ts_ =
  pictures (map (drawTile spr clock) ts_)

drawTile :: Sprites -> Float -> Tile -> Picture
drawTile spr clock t = translate tx ty pic
  where
    tx  = fromIntegral (tCol t) * ts + ts/2
    ty  = fromIntegral (tRow t) * ts + ts/2
    pic = case tType t of
      Ground     -> scale tileScale tileScale (spBlockGround spr)
      Brick      -> scale tileScale tileScale (spBlockBrick spr)
      QBlock     -> scale tileScale tileScale (qBlockFrame spr clock)
      Used       -> scale tileScale tileScale (spBlockHitEmpty spr)
      PipeTop    -> drawPipeTop
      Pipe       -> drawPipe
      PipeR      -> blank
      FlagPole   -> drawFlagPole
      FlagBase   -> drawFlagBase
      Castle     -> drawCastle t
      SlopeLeft  -> scale tileScale tileScale (spBlockGround spr)
      SlopeRight -> scale tileScale tileScale (spBlockGround spr)
      Axe        -> drawAxe
      _          -> blank

-- ? block cycles through 3 frames. The "shine" frame (3) shows briefly.
-- Full cycle: 1,1,1,1,2,2,2,2,3,3,1,1... — frame 3 is the quick flash.
qBlockFrame :: Sprites -> Float -> Picture
qBlockFrame spr clock =
  let frame = (floor (clock * 4) :: Int) `mod` 10
  in case frame of
       8 -> spBlockQuestion3 spr   -- flash frame (brief shine)
       9 -> spBlockQuestion3 spr
       f | f < 5 -> spBlockQuestion1 spr
       _          -> spBlockQuestion2 spr

drawAxe :: Picture
drawAxe = pictures
  [ color (makeColorI 139 69 19 255) (translate 0 (-8) (rectangleSolid 6 20))
  , color (makeColorI 255 215 0 255) (translate 0 8 (polygon [(-10,0),(10,0),(0,12)]))
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
           [ translate 0       (ts*0.25) (rectangleSolid ts 2)
           , translate (ts*0.3) 0        (rectangleSolid 2 ts)
           ]
       , if isBattlement
           then color (makeColorI 100 36 8 255)
                  (translate 0 (ts/2-3) (rectangleSolid (ts*0.5) 6))
           else blank
       ]

-- Piranha — no sprite, keep primitive
drawPiranha :: Picture
drawPiranha = pictures
  [ color (makeColorI 0 180 0 255)   (circleSolid (ts*0.45))
  , color (makeColorI 220 50 50 255) (translate 0 (ts*0.2) (circleSolid (ts*0.2)))
  , color white (translate (-6) 6 (circleSolid 4))
  , color white (translate  (6) 6 (circleSolid 4))
  , color black (translate (-6) 6 (circleSolid 2))
  , color black (translate  (6) 6 (circleSolid 2))
  , color (makeColorI 0 150 0 255)
      (translate 0 (-ts*0.3) (rectangleSolid (ts*0.25) (ts*0.5)))
  , color (makeColorI 0 200 0 255)
      (translate (-ts*0.3) (-ts*0.1) (ellipseS (ts*0.2) (ts*0.15)))
  , color (makeColorI 0 200 0 255)
      (translate (ts*0.3) (-ts*0.1) (ellipseS (ts*0.2) (ts*0.15)))
  ]

-- Firebars — no sprite, keep primitive
drawFirebars :: [Firebar] -> Picture
drawFirebars = pictures . map drawFirebar

drawFirebar :: Firebar -> Picture
drawFirebar fb = pictures
  [ drawFireball (fbX fb + dx) (fbY fb + dy)
  | i <- [0..fbLength fb - 1]
  , let spacing = ts * 0.8
        angle   = fbAngle fb
        dx = spacing * fromIntegral i * cos angle
        dy = spacing * fromIntegral i * sin angle
  ]

drawFireball :: Float -> Float -> Picture
drawFireball x y =
  translate x y $ color (makeColorI 255 100 0 255) (circleSolid (ts*0.3))

-- Power-ups — now sprite-based
drawPups :: Sprites -> [PUp] -> Picture
drawPups spr = pictures . map (drawPup spr)

drawPup :: Sprites -> PUp -> Picture
drawPup spr p
  | not (pAlive p) = blank
  | otherwise      = translate (pX p + ts/2) (pY p)
                       (spMushroom spr)

-- Coins — now sprite-based with 4-frame rotation animation
drawCoins :: Sprites -> Float -> [(Float,Float,Bool)] -> Picture
drawCoins spr clock = pictures . map (drawCoin spr clock)

drawCoin :: Sprites -> Float -> (Float,Float,Bool) -> Picture
drawCoin _   _     (_,_,True) = blank
drawCoin spr clock (x,y,_)   =
  translate x y (coinFrame spr clock)

-- Coin rotates through 4 frames. Each frame holds for ~8 ticks.
coinFrame :: Sprites -> Float -> Picture
coinFrame spr clock =
  let frame = (floor (clock * 8) :: Int) `mod` 4
  in case frame of
       0 -> spCoin1 spr
       1 -> spCoin2 spr
       2 -> spCoin3 spr
       _ -> spCoin4 spr

-- ─── HUD & Overlay (unchanged) ────────────────────────────────────────────────

-- ─── HUD ─────────────────────────────────────────────────────────────────────
-- Five evenly-spaced columns across the top of the 800px window,
-- matching the NES layout: SCORE | COINS | WORLD | TIME | LIVES
-- Each column has a white label on top and a white value below.
-- All positions are in screen coords (not affected by worldYOffset).

hudLabelY :: Float
hudLabelY = 265   -- near window top (+300)

hudValueY :: Float
hudValueY = 243   -- 22px below label

hudScale :: Float
hudScale = 0.16

-- Column centre x positions (evenly split across 800px, -400 to +400)
hudCol :: Int -> Float
hudCol i = -320 + fromIntegral i * 160   -- cols at -320,-160,0,+160,+320

hudLabel :: String -> Float -> Picture
hudLabel s x = translate (x - labelOffset s) hudLabelY
             $ color white
             $ scale hudScale hudScale (text s)
  where
    -- approximate half-width of string to centre it
    labelOffset str = fromIntegral (length str) * hudScale * 52 / 2

hudValue :: String -> Float -> Picture
hudValue s x = translate (x - labelOffset s) hudValueY
             $ color white
             $ scale hudScale hudScale (text s)
  where
    labelOffset str = fromIntegral (length str) * hudScale * 52 / 2

drawHUD :: GS -> Picture
drawHUD gs =
  let currentLevel = gLevels gs !! gLevelIdx gs
      worldNum = lWorld currentLevel
      lvlNum   = lNumber currentLevel
      -- Gloss `text` char width ≈ 104 units at scale 1.0.
      -- We use a helper that centres each string over its column.
  in pictures
       [ hudLabel "SCORE" (hudCol 0)
       , hudLabel "COINS" (hudCol 1)
       , hudLabel "WORLD" (hudCol 2)
       , hudLabel "TIME"  (hudCol 3)
       , hudLabel "LIVES" (hudCol 4)
       , hudValue (show (gScore gs))                       (hudCol 0)
       , hudValue (show (gCoins' gs))                      (hudCol 1)
       , hudValue (show worldNum ++ "-" ++ show lvlNum)    (hudCol 2)
       , hudValue (show (floor (gTimer gs) :: Int))                (hudCol 3)
       , hudValue (show (gLives gs))                       (hudCol 4)
       ]

-- Count collected coins from the coin list (True = collected)
gCoins' :: GS -> Int
gCoins' gs = length (filter (\(_,_,c) -> c) (gCoins gs))

drawOverlay :: GS -> Picture
drawOverlay gs = case gPhase gs of
  Play -> blank
  Over -> mkOv (dark red)                 "GAME OVER" ("Lives: " ++ show (gLives gs))
  Win  -> mkOv (makeColorI 255 215 0 255) "YOU WIN!"  ("Score: " ++ show (gScore gs))
  _    -> blank
  where
    mkOv c t1 t2 = pictures
      [ color (withAlpha 0.65 black) (rectangleSolid 900 700)
      , color c     (translate (-155)   40  (scale 0.45 0.45 (text t1)))
      , color white (translate (-160) (-25) (scale 0.2  0.2  (text t2)))
      , color white (translate (-110) (-65) (scale 0.16 0.16 (text "Press R to restart")))
      ]
