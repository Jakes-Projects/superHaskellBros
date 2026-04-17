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
  , spBlockStep        :: Picture
    -- Pipe (single sprite, scaled to fit height)
  , spPipe             :: Picture
    -- Decorations
  , spCloudSingle :: Picture
  , spCloudDouble :: Picture
  , spBushSingle  :: Picture
  , spBushTriple  :: Picture
  , spHillSmall   :: Picture
  , spHillLarge   :: Picture
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
  <*> loadPNG "assets/block_step.png"
  -- Pipe
  <*> loadPNG "assets/pipe.png"
  -- Decorations
  <*> loadPNG "assets/cloud_single.png"
  <*> loadPNG "assets/cloud_double.png"
  <*> loadPNG "assets/bush_single.png"
  <*> loadPNG "assets/bush_triple.png"
  <*> loadPNG "assets/hill_small.png"
  <*> loadPNG "assets/hill_large.png"
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
      [ drawDecorations spr                                         -- background
      , drawTilesOfType spr clock isGround   (gTiles gs)           -- ground (covers deco bases)
      , drawTilesOfType spr clock (not.isGround) (gTiles gs)       -- pipes/blocks on top
      , drawCoins   spr clock (gCoins gs)
      , drawPups    spr       (gPups  gs)
      , drawFirebars          (gFirebars gs)
      , drawEnem    spr clock (gEnem  gs)
      , drawMario   spr       (gMario gs)
      ]

isGround :: Tile -> Bool
isGround t = tType t == Ground

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
  Koopa   -> scale (marioScale * koopaFace e) marioScale $ koopaFrame spr clock e
  Piranha -> translate 0 (ts * 0.6) drawPiranha

-- Koopa sprite faces right by default. Flip when moving left (eVX < 0).
koopaFace :: Enemy -> Float
koopaFace e = if eVX e >= 0 then 1 else -1

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

-- ─── Decorations ─────────────────────────────────────────────────────────────
-- All decoration sprites are pre-scaled (3x NES native).
-- We render them at 1:1 (no further scaling) since their pixel size already
-- matches the world coordinate scale.

-- Cloud positions: (col, y-row, size) where size 1=single, 2=double
cloudPositions :: [(Float, Float, Int)]
cloudPositions =
  [ (6,10,1), (24,10,2), (44,10,1), (64,10,2)
  , (80,10,1), (98,10,2), (118,10,1), (136,10,1)
  , (154,10,2), (172,10,1), (192,10,1)
  ]

-- Hill positions: (col, _). Alternate small/large by index.
hillPositions :: [(Int, Float, Float)]
hillPositions = zip3 [0..]
  [0, 16, 48, 80, 96, 128, 160, 192]
  (repeat 1)

-- Bush positions: (col, _). Alternate single/triple by index.
bushPositions :: [(Int, Float, Float)]
bushPositions = zip3 [0..]
  [11, 23, 35, 57, 73, 89, 105, 121, 145, 170, 185]
  (repeat 1)

drawDecorations :: Sprites -> Picture
drawDecorations spr = pictures $
  map (drawCloud spr) cloudPositions ++
  map (drawHill  spr) hillPositions  ++
  map (drawBush  spr) bushPositions

drawCloud :: Sprites -> (Float, Float, Int) -> Picture
drawCloud spr (c, _, sz) =
  let x = c * ts + ts/2
      y = 10 * ts
      pic = if sz == 1 then spCloudSingle spr else spCloudDouble spr
  in translate x y pic

drawHill :: Sprites -> (Int, Float, Float) -> Picture
drawHill spr (idx, c, _) =
  let x   = c * ts + ts
      pic = if even idx then spHillSmall spr else spHillLarge spr
      -- Lower by ts/2 so the base overlaps ground tiles (matches NES look).
      -- hill_small ≈ 96px tall → half = 48. Center = ts + 48 - ts/2 = 64.
      -- hill_large ≈ 144px tall → half = 72. Center = ts + 72 - ts/2 = 88.
      y   = if even idx then ts + 32 else ts + 56
  in translate x y pic

drawBush :: Sprites -> (Int, Float, Float) -> Picture
drawBush spr (idx, c, _) =
  let x   = c * ts
      pic = if even idx then spBushSingle spr else spBushTriple spr
      -- bush ≈ 48px tall → half = 24. Center = ts + 24 - ts/2 = 40.
      y   = ts + 8
  in translate x y pic

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

drawTilesOfType :: Sprites -> Float -> (Tile -> Bool) -> [Tile] -> Picture
drawTilesOfType spr clock p ts_ =
  pictures (map (drawTile spr clock) (filter p ts_))

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
      Step       -> scale tileScale tileScale (spBlockStep spr)
      -- Pipes: pipe.png (48x96) rendered ONCE at PipeTop, covering the full pipe.
      -- Pipe spans world y = ts (ground surface) to (h+1)*ts (top of PipeTop tile).
      -- Pipe height = h*ts. Center y = ts + h*ts/2.
      -- drawTile already translates to ty = h*ts + ts/2 (PipeTop tile centre).
      -- offsetY = desired_centre - tile_centre = (ts + h*ts/2) - (h*ts + ts/2) = ts*(1-h)/2
      -- scaleX = 2*ts/48  (two tiles wide)
      -- scaleY = h*ts/96  (stretches to full pipe height)
      PipeTop    ->
        let h       = fromIntegral (tRow t) :: Float
            scaleX  = 2 * ts / 48
            scaleY  = h * ts / 96
            offsetY = ts * (1 - h) / 2
        in translate (ts/2) offsetY $ scale scaleX scaleY (spPipe spr)
      Pipe       -> blank
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
      (translate (-ts*0.3) (-ts*0.1) (scale (ts*0.2) (ts*0.15) (circleSolid 1)))
  , color (makeColorI 0 200 0 255)
      (translate (ts*0.3) (-ts*0.1) (scale (ts*0.2) (ts*0.15) (circleSolid 1)))
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
