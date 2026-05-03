module Rendering (Sprites, loadSprites, draw) where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Constants (ts, sW, sH)
import Types

-- ─── Sprite record ────────────────────────────────────────────────────────────

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
    -- Fire Mario  (reuses big-mario PNGs with a white palette if no dedicated
    --              asset exists; loadPNG falls back to a magenta placeholder so
    --              the game still runs without them)
  , spFireStand   :: Picture
  , spFireRun1    :: Picture
  , spFireRun2    :: Picture
  , spFireRun3    :: Picture
  , spFireJump    :: Picture
  , spFireSkid    :: Picture
  , spFireCrouch  :: Picture
    -- Goomba
  , spGoomba1       :: Picture
  , spGoomba2       :: Picture
  , spGoombaCrushed :: Picture
    -- Koopa
  , spKoopa1         :: Picture
  , spKoopa2         :: Picture
  , spKoopaShell     :: Picture
  , spKoopaResetting :: Picture
    -- Bowser
  , spBowser1     :: Picture
  , spBowser2     :: Picture
  , spBowser3     :: Picture
  , spBowser4     :: Picture
  , spBowserFire1 :: Picture
  , spBowserFire2 :: Picture
    -- Blocks
  , spBlockGround      :: Picture
  , spBlockBrick       :: Picture
  , spBlockBrickBroken :: Picture
  , spBlockHitEmpty    :: Picture
  , spBlockQuestion1   :: Picture
  , spBlockQuestion2   :: Picture
  , spBlockQuestion3   :: Picture
  , spBlockStep        :: Picture
    -- Pipe
  , spPipe             :: Picture
    -- Decorations
  , spCloudSingle :: Picture
  , spCloudDouble :: Picture
  , spBushSingle  :: Picture
  , spBushTriple  :: Picture
  , spHillSmall   :: Picture
  , spHillLarge   :: Picture
    -- Collectibles
  , spMushroom    :: Picture
  , spFireFlower1 :: Picture
  , spFireFlower2 :: Picture
  , spFireFlower3 :: Picture
  , spFireFlower4 :: Picture
  , spFireball1   :: Picture
  , spFireball2   :: Picture
  , spFireball3   :: Picture
  , spFireball4   :: Picture
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
      return $ color magenta (rectangleSolid 32 32)

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
  -- Fire Mario (dedicated assets; falls back gracefully if missing)
  <*> loadPNG "assets/mario_fire_stand.png"
  <*> loadPNG "assets/mario_fire_run_1.png"
  <*> loadPNG "assets/mario_fire_run_2.png"
  <*> loadPNG "assets/mario_fire_run_3.png"
  <*> loadPNG "assets/mario_fire_jump.png"
  <*> loadPNG "assets/mario_fire_skid.png"
  <*> loadPNG "assets/mario_fire_crouch.png"
  -- Goomba
  <*> loadPNG "assets/goomba_1.png"
  <*> loadPNG "assets/goomba_2.png"
  <*> loadPNG "assets/goomba_crushed.png"
  -- Koopa
  <*> loadPNG "assets/koopa_green_1.png"
  <*> loadPNG "assets/koopa_green_2.png"
  <*> loadPNG "assets/koopa_green_shell.png"
  <*> loadPNG "assets/koopa_green_resetting.png"
  -- Bowser
  <*> loadPNG "assets/bowser_1.png"
  <*> loadPNG "assets/bowser_2.png"
  <*> loadPNG "assets/bowser_3.png"
  <*> loadPNG "assets/bowser_4.png"
  <*> loadPNG "assets/bowser_fire_1.png"
  <*> loadPNG "assets/bowser_fire_2.png"
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
  <*> loadPNG "assets/fire_flower_1.png"
  <*> loadPNG "assets/fire_flower_2.png"
  <*> loadPNG "assets/fire_flower_3.png"
  <*> loadPNG "assets/fire_flower_4.png"
  <*> loadPNG "assets/fireball_1.png"
  <*> loadPNG "assets/fireball_2.png"
  <*> loadPNG "assets/fireball_3.png"
  <*> loadPNG "assets/fireball_4.png"
  <*> loadPNG "assets/coin_1.png"
  <*> loadPNG "assets/coin_2.png"
  <*> loadPNG "assets/coin_3.png"
  <*> loadPNG "assets/coin_4.png"

-- ─── World Y offset ──────────────────────────────────────────────────────────
worldYOffset :: Float
worldYOffset = -(fromIntegral sH / 2) + 3.0 * ts - ts   -- = -236

-- ─── Top-level draw ───────────────────────────────────────────────────────────

draw :: Sprites -> GS -> IO Picture
draw spr gs = return $ pictures
  [ drawSky
  , translate (-(gCam gs)) worldYOffset world
  , drawHUD gs
  , drawOverlay gs
  ]
  where
    clock = mAnim (gMario gs)
    world = pictures
      [ drawDecorations spr
      , drawTilesOfType spr clock isGround      (gTiles gs)
      , drawTilesOfType spr clock (not.isGround) (gTiles gs)
      , drawCoins   spr clock (gCoins gs)
      , drawPups    spr clock (gPups  gs)
      , drawFirebars spr clock (gFirebars gs)
      , drawPlayerFireballs spr clock (gFireballs gs)
      , drawEnem    spr clock (gEnem  gs)
      , drawMario   spr       (gMario gs)
      ]

isGround :: Tile -> Bool
isGround t = tType t == Ground

-- ─── Mario ────────────────────────────────────────────────────────────────────

marioScale :: Float
marioScale = 1.0

drawMario :: Sprites -> Mario -> Picture
drawMario spr m
  | mState m == MDead =
      translate (mX m) (mY m)
        $ scale marioScale marioScale
        $ spMarioDeath spr
  | blink = blank
  | otherwise =
      translate (mX m) drawY
        $ scale (marioScale * fromIntegral (mFace m)) marioScale
        $ pickMarioFrame spr m
  where
    blink  = mInv m > 0 && even (floor (mInv m * 10) :: Int)
    -- When crouching, Big/Fire Mario's BB shrinks to small height (ts instead of ts*2).
    -- The physics center mY stays put, so the sprite floats ts/2 above the ground.
    -- Shift the sprite down by ts/2 to keep the feet at the correct world position.
    drawY  = if mCrouch m then mY m - ts/2 else mY m

pickMarioFrame :: Sprites -> Mario -> Picture
pickMarioFrame spr m =
  let airborne = not (mGround m)
      wFrame   = (floor (mAnim m * 10) :: Int) `mod` 3
      skidding = (mFace m == 1    && mVX m < -10)
              || (mFace m == (-1) && mVX m >  10)
      still    = abs (mVX m) < 5 && mGround m
      crouching = mCrouch m
  in case mState m of
       Big   -> pickBigFrame   spr airborne skidding still crouching wFrame
       Fire  -> pickFireFrame  spr airborne skidding still crouching wFrame
       _     -> pickSmallFrame spr airborne skidding still wFrame

-- Small Mario has no crouch sprite — crouch only applies to Big/Fire
pickSmallFrame :: Sprites -> Bool -> Bool -> Bool -> Int -> Picture
pickSmallFrame spr airborne skidding still wFrame
  | airborne  = spMarioJump  spr
  | skidding  = spMarioSkid  spr
  | still     = spMarioStand spr
  | wFrame == 0 = spMarioRun1 spr
  | wFrame == 1 = spMarioRun2 spr
  | otherwise   = spMarioRun3 spr

pickBigFrame :: Sprites -> Bool -> Bool -> Bool -> Bool -> Int -> Picture
pickBigFrame spr airborne skidding still crouching wFrame
  | crouching   = spBigCrouch spr
  | airborne    = spBigJump   spr
  | skidding    = spBigSkid   spr
  | still       = spBigStand  spr
  | wFrame == 0 = spBigRun1   spr
  | wFrame == 1 = spBigRun2   spr
  | otherwise   = spBigRun3   spr

pickFireFrame :: Sprites -> Bool -> Bool -> Bool -> Bool -> Int -> Picture
pickFireFrame spr airborne skidding still crouching wFrame
  | crouching   = spFireCrouch spr
  | airborne    = spFireJump   spr
  | skidding    = spFireSkid   spr
  | still       = spFireStand  spr
  | wFrame == 0 = spFireRun1   spr
  | wFrame == 1 = spFireRun2   spr
  | otherwise   = spFireRun3   spr

-- ─── Enemies ──────────────────────────────────────────────────────────────────

drawEnem :: Sprites -> Float -> [Enemy] -> Picture
drawEnem spr clock = pictures . map (drawE spr clock)

drawE :: Sprites -> Float -> Enemy -> Picture
drawE spr clock e = case eState e of
  EDead _         -> translate cx (eY e + 5)          (spGoombaCrushed spr)
  EShell _ moving -> translate cx (eY e + spriteHalf) (shellPic moving)
  EBowser _ _ _   -> translate cx (eY e + spriteHalf) (drawEnemyBody spr clock e)
  _               ->
    if shouldDrawAlive (eState e)
      then translate cx (eY e + spriteHalf) (drawEnemyBody spr clock e)
      else blank
  where
    cx = eX e + ts/2
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
  Bowser  -> drawBowser spr clock e

-- Bowser cycles through 4 walk frames at half speed (clock * 4 mod 4).
-- Sprite faces LEFT by default — flip when moving RIGHT (eVX > 0).
drawBowser :: Sprites -> Float -> Enemy -> Picture
drawBowser spr clock e =
  let frame   = (floor (clock * 4) :: Int) `mod` 4
      walkPic = case frame of
                  0 -> spBowser1 spr
                  1 -> spBowser2 spr
                  2 -> spBowser3 spr
                  _ -> spBowser4 spr
      facing  = if eVX e > 0 then -1 else 1 :: Float
  in scale (marioScale * facing) marioScale walkPic

koopaFace :: Enemy -> Float
koopaFace e = if eVX e >= 0 then 1 else -1

goombaFrame :: Sprites -> Float -> Picture
goombaFrame spr clock =
  if even (floor (clock * 8) :: Int)
    then spGoomba1 spr
    else spGoomba2 spr

koopaFrame :: Sprites -> Float -> Enemy -> Picture
koopaFrame spr clock e = case eState e of
  EShell _ _ -> spKoopaShell spr
  _ ->
    if even (floor (clock * 8) :: Int)
      then spKoopa1 spr
      else spKoopa2 spr

-- ─── Primitives ───────────────────────────────────────────────────────────────

drawSky :: Picture
drawSky = color skyBlue (rectangleSolid (fromIntegral sW) (fromIntegral sH))

skyBlue :: Color
skyBlue = makeColorI 97 133 248 255

-- ─── Decorations ─────────────────────────────────────────────────────────────
-- All positions derived from pixel-by-pixel analysis of the NES reference image.
-- Clouds alternate between two heights: ~306 (lower row) and ~338 (upper row).
-- Size 1 = single cloud sprite, size 2 = double cloud sprite.

cloudPositions :: [(Float, Float, Int)]
cloudPositions =
  [ (  8, 306, 1 )   -- single, lower
  , ( 19, 338, 1 )   -- single, upper
  , ( 27, 306, 2 )   -- double, lower
  , ( 36, 338, 2 )   -- double, upper
  , ( 56, 306, 1 )
  , ( 67, 338, 1 )
  , ( 75, 306, 2 )
  , ( 84, 338, 2 )
  , (104, 306, 1 )
  , (115, 338, 1 )
  , (123, 306, 2 )
  , (132, 338, 2 )
  , (152, 306, 1 )
  , (163, 338, 1 )
  , (171, 306, 2 )
  , (180, 338, 2 )
  , (197, 320, 2 )
  ]

-- Hills: large mounds use spHillLarge, small mounds use spHillSmall.
-- Each entry: (col_anchor, isLarge)
hillPositions :: [(Float, Bool)]
hillPositions =
  [ (  0, True  )   -- large, start of level
  , ( 38, False )   -- small
  , ( 46, True  )   -- large, wide mound
  , ( 96, True  )   -- large
  , (144, False )   -- small
  , (192, True  )   -- large, near end
  ]

-- Bushes: single shrub or triple shrub.
-- Each entry: (col_anchor, isTriple)
bushPositions :: [(Float, Bool)]
bushPositions =
  [ ( 12, False )
  , ( 16, False )
  , ( 28, False )
  , ( 42, False )
  , ( 60, False )
  , ( 64, False )
  -- col 90 bush omitted: sits 1 tile from pit edge (cols 86-88), bleeds visually over gap
  , (108, False )
  , (112, False )
  , (138, False )
  , (160, True  )   -- triple bush
  , (179, False )
  , (208, False )
  ]

drawDecorations :: Sprites -> Picture
drawDecorations spr = pictures $
  map (drawCloud spr) cloudPositions ++
  map (drawHill  spr) hillPositions  ++
  map (drawBush  spr) bushPositions

drawCloud :: Sprites -> (Float, Float, Int) -> Picture
drawCloud spr (c, gameY, sz) =
  let x   = c * ts + ts/2
      pic = if sz == 1 then spCloudSingle spr else spCloudDouble spr
  in translate x gameY pic

drawHill :: Sprites -> (Float, Bool) -> Picture
drawHill spr (c, isLarge) =
  let x   = c * ts + ts
      pic = if isLarge then spHillLarge spr else spHillSmall spr
      y   = if isLarge then ts + 56 else ts + 32
  in translate x y pic

drawBush :: Sprites -> (Float, Bool) -> Picture
drawBush spr (c, isTriple) =
  let x   = c * ts
      pic = if isTriple then spBushTriple spr else spBushSingle spr
  in translate x (ts + 8) pic

-- ─── Tiles ────────────────────────────────────────────────────────────────────

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

qBlockFrame :: Sprites -> Float -> Picture
qBlockFrame spr clock =
  let frame = (floor (clock * 4) :: Int) `mod` 10
  in case frame of
       8 -> spBlockQuestion3 spr
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
           [ translate 0        (ts*0.25) (rectangleSolid ts 2)
           , translate (ts*0.3) 0         (rectangleSolid 2 ts)
           ]
       , if isBattlement
           then color (makeColorI 100 36 8 255)
                  (translate 0 (ts/2-3) (rectangleSolid (ts*0.5) 6))
           else blank
       ]

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

drawFirebars :: Sprites -> Float -> [Firebar] -> Picture
drawFirebars spr clock = pictures . map (drawFirebar spr clock)

drawFirebar :: Sprites -> Float -> Firebar -> Picture
drawFirebar spr clock fb = pictures
  [ drawFireball spr clock (fbX fb + dx) (fbY fb + dy)
  | i <- [0..fbLength fb - 1]
  , let spacing = ts * 0.8
        angle   = fbAngle fb
        dx = spacing * fromIntegral i * cos angle
        dy = spacing * fromIntegral i * sin angle
  ]

drawFireball :: Sprites -> Float -> Float -> Float -> Picture
drawFireball spr clock x y =
  translate x y (fireballFrame spr clock)

fireballFrame :: Sprites -> Float -> Picture
fireballFrame spr clock =
  let frame = (floor (clock * 8) :: Int) `mod` 4
  in case frame of
       0 -> spFireball1 spr
       1 -> spFireball2 spr
       2 -> spFireball3 spr
       _ -> spFireball4 spr

drawPlayerFireballs :: Sprites -> Float -> [Fireball] -> Picture
drawPlayerFireballs spr clock = pictures . map (drawPlayerFireball spr clock)

drawPlayerFireball :: Sprites -> Float -> Fireball -> Picture
drawPlayerFireball spr clock fb
  | not (fiAlive fb) = blank
  | fiBowser fb      = translate (fiX fb) (fiY fb)
                         $ scale (if fiVX fb > 0 then -1 else 1) 1
                         $ bowserFireFrame spr clock
  | otherwise        = translate (fiX fb) (fiY fb) (fireballFrame spr clock)

-- Bowser fire cycles between the two bowser_fire sprites at 8fps
bowserFireFrame :: Sprites -> Float -> Picture
bowserFireFrame spr clock =
  if even (floor (clock * 8) :: Int)
    then spBowserFire1 spr
    else spBowserFire2 spr

-- ─── Power-ups ────────────────────────────────────────────────────────────────

drawPups :: Sprites -> Float -> [PUp] -> Picture
drawPups spr clock = pictures . map (drawPup spr clock)

drawPup :: Sprites -> Float -> PUp -> Picture
drawPup spr clock p
  | not (pAlive p) = blank
  | otherwise      = translate (pX p + ts/2) (pY p) pic
  where
    pic = case pType p of
            Mushroom   -> spMushroom spr
            FireFlower -> fireFlowerFrame spr clock
            Star       -> spMushroom spr

fireFlowerFrame :: Sprites -> Float -> Picture
fireFlowerFrame spr clock =
  let frame = (floor (clock * 8) :: Int) `mod` 4
  in case frame of
       0 -> spFireFlower1 spr
       1 -> spFireFlower2 spr
       2 -> spFireFlower3 spr
       _ -> spFireFlower4 spr

-- Coins
drawCoins :: Sprites -> Float -> [(Float,Float,Bool)] -> Picture
drawCoins spr clock = pictures . map (drawCoin spr clock)

drawCoin :: Sprites -> Float -> (Float,Float,Bool) -> Picture
drawCoin _   _     (_,_,True) = blank
drawCoin spr clock (x,y,_)   = translate x y (coinFrame spr clock)

coinFrame :: Sprites -> Float -> Picture
coinFrame spr clock =
  let frame = (floor (clock * 8) :: Int) `mod` 4
  in case frame of
       0 -> spCoin1 spr
       1 -> spCoin2 spr
       2 -> spCoin3 spr
       _ -> spCoin4 spr

-- ─── HUD ─────────────────────────────────────────────────────────────────────

hudLabelY :: Float
hudLabelY = 265

hudValueY :: Float
hudValueY = 243

hudScale :: Float
hudScale = 0.16

hudCol :: Int -> Float
hudCol i = -320 + fromIntegral i * 160

hudLabel :: String -> Float -> Picture
hudLabel s x = translate (x - labelOffset s) hudLabelY
             $ color white
             $ scale hudScale hudScale (text s)
  where
    labelOffset str = fromIntegral (length str) * hudScale * 52 / 2

hudValue :: Color -> String -> Float -> Picture
hudValue c s x = translate (x - labelOffset s) hudValueY
               $ color c
               $ scale hudScale hudScale (text s)
  where
    labelOffset str = fromIntegral (length str) * hudScale * 52 / 2

-- | Zero-pad an integer to a minimum number of digits.
zeroPad :: Int -> Int -> String
zeroPad digits n = let s = show n
                       pad = replicate (max 0 (digits - length s)) '0'
                   in pad ++ s

drawHUD :: GS -> Picture
drawHUD gs =
  let currentLevel = gLevels gs !! gLevelIdx gs
      worldNum  = lWorld currentLevel
      lvlNum    = lNumber currentLevel
      timerVal  = floor (gTimer gs) :: Int
      -- Timer turns red when below 100 (NES urgency cue)
      timerColor = if timerVal < 100 then red else white
  in pictures
       [ hudLabel "MARIO"  (hudCol 0)
       , hudLabel "COINS"  (hudCol 1)
       , hudLabel "WORLD"  (hudCol 2)
       , hudLabel "TIME"   (hudCol 3)
       , hudLabel "LIVES"  (hudCol 4)
       , hudValue white  (zeroPad 6 (gScore gs))                   (hudCol 0)
       , hudValue white  ("\xd7" ++ zeroPad 2 (gCoinCount gs))     (hudCol 1)
       , hudValue white  (show worldNum ++ "-" ++ show lvlNum)     (hudCol 2)
       , hudValue timerColor (zeroPad 3 timerVal)                  (hudCol 3)
       , hudValue white  (zeroPad 2 (gLives gs))                   (hudCol 4)
       ]

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