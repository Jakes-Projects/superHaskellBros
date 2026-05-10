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
      -- Cheep-cheep
  , spCheepRed1   :: Picture
  , spCheepRed2   :: Picture
    -- Blooper
  , spBlooper1    :: Picture
  , spBlooper2    :: Picture
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
  , spPipe90           :: Picture
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
    -- Fire Mario shoot sprite
  , spFireShoot   :: Picture
    -- Moving platform
  , spPlatform    :: Picture
    -- Underground variants
  , spUgBlockGround      :: Picture
  , spUgBlockBrick       :: Picture
  , spUgBlockBrickBroken :: Picture
  , spUgBlockHitEmpty    :: Picture
  , spUgBlockStep        :: Picture
  , spUgGoomba1          :: Picture
  , spUgGoomba2          :: Picture
  , spUgGoombaCrushed    :: Picture
  , spUgKoopa1           :: Picture
  , spUgKoopa2           :: Picture
  , spUgKoopaShell       :: Picture
  , spUgKoopaResetting   :: Picture
    -- Castle level sprites
  , spCastleBrick  :: Picture   -- castle_bricks.png  48x48
  , spCastleAxe    :: Picture   -- castle_axe.png     36x40
  , spCastleBridge :: Picture   -- castle_bridge.png  624x64
  , spLava         :: Picture   -- lava.png           144x72
    -- Piranha Plant (overworld + underground)
  , spPiranha1     :: Picture
  , spPiranha2     :: Picture
  , spUgPiranha1   :: Picture
  , spUgPiranha2   :: Picture
    -- Joe Fire Mario sprites (activated by typing "joe" as Fire Mario)
  , spJoeStand    :: Picture
  , spJoeRun1     :: Picture
  , spJoeRun2     :: Picture
  , spJoeRun3     :: Picture
  , spJoeJump     :: Picture
  , spJoeCrouch   :: Picture
  , spJoeSkid     :: Picture
  , spJoeShoot    :: Picture
  , spJoeFireball1 :: Picture
  , spJoeFireball2 :: Picture
  , spJoeFireball3 :: Picture
  , spJoeFireball4 :: Picture
    -- Underwater-specific sprites
  , spUwTile      :: Picture   -- underwater_tile.png
  , spCoral       :: Picture   -- coral.png
  , spWater       :: Picture   -- water.png (wave strip)
  , spCoinExposed :: Picture   -- coin_exposed.png
  , spCheepGreen1 :: Picture   -- cheep_green_1.png
  , spCheepGreen2 :: Picture   -- cheep_green_2.png
    -- Swim sprites (cycled through on each stroke, frames 1-5)
  , spMarioSwim1  :: Picture
  , spMarioSwim2  :: Picture
  , spMarioSwim3  :: Picture
  , spMarioSwim4  :: Picture
  , spMarioSwim5  :: Picture
  , spBigSwim1    :: Picture
  , spBigSwim2    :: Picture
  , spBigSwim3    :: Picture
  , spBigSwim4    :: Picture
  , spBigSwim5    :: Picture
  , spFireSwim1   :: Picture
  , spFireSwim2   :: Picture
  , spFireSwim3   :: Picture
  , spFireSwim4   :: Picture
  , spFireSwim5   :: Picture
  , spJoeSwim1    :: Picture
  , spJoeSwim2    :: Picture
  , spJoeSwim3    :: Picture
  , spJoeSwim4    :: Picture
  , spJoeSwim5    :: Picture
    -- Overworld end-of-level sprites
  , spCastleLevelEnd :: Picture   -- castle_level_end.png  (replaces drawn castle tiles)
  , spFlagpole       :: Picture   -- flagpole.png           (replaces drawn pole)
  , spFlag           :: Picture   -- flag.png               (slides down the pole)
    -- Flagpole slide sprites (one per Mario form)
  , spMarioSlide  :: Picture   -- mario_slide.png
  , spBigSlide    :: Picture   -- mario_big_slide.png
  , spFireSlide   :: Picture   -- mario_fire_slide.png
  , spJoeSlide    :: Picture   -- joe_fire_slide.png
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
    -- Cheep-cheep
  <*> loadPNG "assets/cheep_red_1.png"
  <*> loadPNG "assets/cheep_red_2.png"
  -- Blooper
  <*> loadPNG "assets/blooper_1.png"
  <*> loadPNG "assets/blooper_2.png"
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
  <*> loadPNG "assets/pipe_90_degree.png"
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
  -- Fire Mario shoot
  <*> loadPNG "assets/mario_fire_fireball_shoot.png"
  -- Moving platform
  <*> loadPNG "assets/platform_moving.png"
  -- Underground variants
  <*> loadPNG "assets/block_ground_ug.png"
  <*> loadPNG "assets/block_brick_ug.png"
  <*> loadPNG "assets/block_brick_broken_ug.png"
  <*> loadPNG "assets/block_hit_empty_ug.png"
  <*> loadPNG "assets/block_step_ug.png"
  <*> loadPNG "assets/goomba_1_ug.png"
  <*> loadPNG "assets/goomba_2_ug.png"
  <*> loadPNG "assets/goomba_crushed_ug.png"
  <*> loadPNG "assets/koopa_green_1_ug.png"
  <*> loadPNG "assets/koopa_green_2_ug.png"
  <*> loadPNG "assets/koopa_green_shell_ug.png"
  <*> loadPNG "assets/koopa_green_resetting_ug.png"
  -- Castle level sprites
  <*> loadPNG "assets/castle_bricks.png"
  <*> loadPNG "assets/castle_axe.png"
  <*> loadPNG "assets/castle_bridge.png"
  <*> loadPNG "assets/lava.png"
  -- Piranha Plant
  <*> loadPNG "assets/piranha_plant_1.png"
  <*> loadPNG "assets/piranha_plant_2.png"
  <*> loadPNG "assets/piranha_plant_1_ug.png"
  <*> loadPNG "assets/piranha_plant_2_ug.png"
  -- Joe Fire Mario
  <*> loadPNG "assets/joe_fire_stand.png"
  <*> loadPNG "assets/joe_fire_run_1.png"
  <*> loadPNG "assets/joe_fire_run_2.png"
  <*> loadPNG "assets/joe_fire_run_3.png"
  <*> loadPNG "assets/joe_fire_jump.png"
  <*> loadPNG "assets/joe_fire_crouch.png"
  <*> loadPNG "assets/joe_fire_skid.png"
  <*> loadPNG "assets/joe_fire_fireball_shoot.png"
  <*> loadPNG "assets/fireball_haskell_1.png"
  <*> loadPNG "assets/fireball_haskell_2.png"
  <*> loadPNG "assets/fireball_haskell_3.png"
  <*> loadPNG "assets/fireball_haskell_4.png"
  -- Underwater
  <*> loadPNG "assets/underwater_tile.png"
  <*> loadPNG "assets/coral.png"
  <*> loadPNG "assets/water.png"
  <*> loadPNG "assets/coin_exposed.png"
  <*> loadPNG "assets/cheep_green_1.png"
  <*> loadPNG "assets/cheep_green_2.png"
  -- Swim sprites
  <*> loadPNG "assets/mario_swim_1.png"
  <*> loadPNG "assets/mario_swim_2.png"
  <*> loadPNG "assets/mario_swim_3.png"
  <*> loadPNG "assets/mario_swim_4.png"
  <*> loadPNG "assets/mario_swim_5.png"
  <*> loadPNG "assets/mario_big_swim_1.png"
  <*> loadPNG "assets/mario_big_swim_2.png"
  <*> loadPNG "assets/mario_big_swim_3.png"
  <*> loadPNG "assets/mario_big_swim_4.png"
  <*> loadPNG "assets/mario_big_swim_5.png"
  <*> loadPNG "assets/mario_fire_swim_1.png"
  <*> loadPNG "assets/mario_fire_swim_2.png"
  <*> loadPNG "assets/mario_fire_swim_3.png"
  <*> loadPNG "assets/mario_fire_swim_4.png"
  <*> loadPNG "assets/mario_fire_swim_5.png"
  <*> loadPNG "assets/joe_fire_swim_1.png"
  <*> loadPNG "assets/joe_fire_swim_2.png"
  <*> loadPNG "assets/joe_fire_swim_3.png"
  <*> loadPNG "assets/joe_fire_swim_4.png"
  <*> loadPNG "assets/joe_fire_swim_5.png"
  -- Overworld end-of-level sprites
  <*> loadPNG "assets/castle_level_end.png"
  <*> loadPNG "assets/flagpole.png"
  <*> loadPNG "assets/flag.png"
  -- Flagpole slide sprites
  <*> loadPNG "assets/mario_slide.png"
  <*> loadPNG "assets/mario_big_slide.png"
  <*> loadPNG "assets/mario_fire_slide.png"
  <*> loadPNG "assets/joe_fire_slide.png"

-- ─── World Y offset ──────────────────────────────────────────────────────────
worldYOffset :: Float
worldYOffset = -(fromIntegral sH / 2) + 3.0 * ts - ts   -- = -236

-- ─── Top-level draw ───────────────────────────────────────────────────────────

isUnderwater :: GS -> Bool
isUnderwater gs =
  let lvl = gLevels gs !! gLevelIdx gs
  in lWorld lvl == 2 && lNumber lvl == 2

isUnderground :: GS -> Bool
isUnderground gs =
  let lvl = gLevels gs !! gLevelIdx gs
  in not (isUnderwater gs) && lNumber lvl == 2

isCastle :: GS -> Bool
isCastle gs =
  let lvl = gLevels gs !! gLevelIdx gs
  in lNumber lvl == 4

draw :: Sprites -> GS -> IO Picture
draw spr gs
  | gPhase gs == LevelIntro = return (drawLevelIntro spr gs)
  | gPhase gs == PipeEntry  = return $ pictures
      [ drawSkyFor gs
      , translate (-(gCam gs)) worldYOffset world
      , drawHUD gs
      ]
  | otherwise = return $ pictures
      [ drawSkyFor gs
      , translate (-(gCam gs)) worldYOffset world
      , if underwater then drawWaveStrip spr gs else blank
      , translate (-(gCam gs)) worldYOffset (drawMario spr (gMario gs))
      , drawHUD gs
      , drawOverlay gs
      ]
  where
    underwater  = isUnderwater gs
    underground = isUnderground gs || underwater
    castle      = isCastle gs
    clock       = mAnim (gMario gs)
    world = pictures
      [ if underground || castle then blank else drawDecorations spr
      , if underwater
          then color waterBlue (translate (gCam gs) (3 * ts + 4)
                 (rectangleSolid (fromIntegral (220 * (32 :: Int))) (fromIntegral sH)))
          else blank
      -- Lava drawn first so floor tiles render on top (prevents clipping)
      , if castle then drawLava spr (gTiles gs) else blank
      -- Piranhas behind all tiles
      , drawEnem    spr underground clock (filter isPiranha (gEnem gs))
      , if isPipeEntry then drawMarioPipeClipped spr gs else blank
      , drawTilesOfType spr underground underwater castle clock flagOff isPipeEntry isGround      anims (gTiles gs)
      , drawTilesOfType spr underground underwater castle clock flagOff isPipeEntry (\t -> not (isGround t) && not (isPipeEntry && tType t == Step)) anims (gTiles gs)
      , drawBrickAnims  spr underground clock anims
      , if underwater then drawCoralTiles spr (gTiles gs) else blank
      -- Bridge rendered over floor tiles at Bowser's pit
      , if castle then drawCastleBridge spr (gTiles gs) else blank
      -- Overworld end-of-level castles drawn as single sprites (non-castle levels)
      , if not castle then drawOverworldCastles spr (gTiles gs) else blank
      -- Flagpole drawn as a sprite pass (non-castle levels)
      , if not castle then drawFlagpoles spr flagOff (gTiles gs) else blank
      , drawPlatforms spr (gPlatforms gs)
      , drawCoins   spr underwater clock (gCoins gs)
      , drawPups    spr clock (gPups  gs)
      , drawFirebars spr clock (gFirebars gs)
      , drawPlayerFireballs spr clock (mJoeMode (gMario gs)) (gFireballs gs)
      , drawEnem    spr underground clock (filter (not . isPiranha) (gEnem gs))
      , if isPipeEntry then blank else drawMario spr (gMario gs)
      ]
    anims   = gBrickAnims gs
    flagOff = gFlagOffset gs
    isPipeEntry = gPhase gs == PipeEntry

-- | Tile water.png across the top of the screen in screen space.
-- The real NES game has a ~2-tile (64px) blue sky strip above the wave.
-- screenY=200 places the sprite so its top is 64px below the screen top edge.
drawWaveStrip :: Sprites -> GS -> Picture
drawWaveStrip spr gs =
  let sprW    = 144 :: Float
      screenY = 200 :: Float   -- top of wave = screenY + sprH/2 = 236, gap = 300-236 = 64px
      nTiles  = ceiling (fromIntegral sW / sprW) + 2 :: Int
      camOff  = gCam gs `fmod` sprW
      startX  = -(fromIntegral sW / 2) - camOff
  in pictures
       [ translate (startX + fromIntegral i * sprW) screenY (spWater spr)
       | i <- [0..nTiles]
       ]
  where
    fmod a b = a - fromIntegral (floor (a / b) :: Int) * b

isGround :: Tile -> Bool
isGround t = tType t == Ground

-- ─── Mario ────────────────────────────────────────────────────────────────────

drawMarioPipeClipped :: Sprites -> GS -> Picture
drawMarioPipeClipped spr gs = drawMario spr (gMario gs)

marioScale :: Float
marioScale = 1.0

drawMario :: Sprites -> Mario -> Picture
drawMario spr m
  | mState m == MDead =
      translate (mX m) (mY m + 8)
        $ scale marioScale marioScale
        $ spMarioDeath spr
  | blink = blank
  | otherwise =
      translate (mX m) drawY
        $ scale (marioScale * fromIntegral (if mSkidding m then -(mFace m) else mFace m)) marioScale
        $ pickMarioFrame spr flashM
  where
    blink  = mInv m > 1.0 && even (floor (mInv m * 10) :: Int)
    -- During power-up transformation: alternate between current and target sprite
    -- at ~12 Hz to produce the classic NES palette-flash effect.
    transforming = mTransformTimer m > 0
    flashM
      | transforming && odd (floor (mTransformTimer m * 12) :: Int)
          = m { mState = mTransformTarget m }
      | otherwise = m
    -- mY is the physics centre (halfH above the bottom edge).
    -- Sprites are taller than the BB, so we shift up to align the sprite bottom
    -- with the physics bottom edge:
    --   Small Mario: sprite 48px, half=24; BB half=ts/2=16 → shift up by 8
    --   Big/Fire/Joe: sprite 96px, half=48; BB half=ts=32  → shift up by 16
    -- Crouching collapses Big/Fire BB to small height, so subtract ts/2 on top.
    -- Use target state for the sprite shift so position doesn't jitter during flash
    shiftState = if transforming then mTransformTarget m else mState m
    spriteShift = case shiftState of
                    Small -> 8
                    _     -> 16
    drawY = if mCrouch m
              then mY m - ts/2 + spriteShift
              else mY m + spriteShift

pickMarioFrame :: Sprites -> Mario -> Picture
pickMarioFrame spr m
  -- Slide sprite overrides everything while on the flagpole
  | mSliding m = case mState m of
      Big  -> spBigSlide  spr
      Fire -> if mJoeMode m then spJoeSlide spr else spFireSlide spr
      _    -> spMarioSlide spr
  | otherwise =
  let airborne  = not (mGround m)
      wFrame    = (floor (mAnim m * 10) :: Int) `mod` 3
      still     = abs (mVX m) < 5 && mGround m
      crouching = mCrouch m
      joe       = mJoeMode m
      shooting  = mFireCool m > 0.2
      swimming  = mSwimming m
      swimFrame = mSwimAnim m
      skidding  = mSkidding m
  in case mState m of
       Big   -> if swimming then pickSwimFrame (bigSwimSprites   spr) swimFrame
                            else pickBigFrame  spr airborne still crouching skidding wFrame
       Fire  -> if swimming then pickSwimFrame (if joe then joeSwimSprites spr
                                                       else fireSwimSprites spr) swimFrame
                            else pickFireFrame spr joe shooting airborne still crouching skidding wFrame
       _     -> if swimming then pickSwimFrame (smallSwimSprites spr) swimFrame
                            else pickSmallFrame spr airborne still skidding wFrame

-- | Select one of 5 swim sprites based on the current stroke frame.
pickSwimFrame :: [Picture] -> Int -> Picture
pickSwimFrame sprites frame = sprites !! (frame `mod` 5)

smallSwimSprites :: Sprites -> [Picture]
smallSwimSprites spr = [spMarioSwim1 spr, spMarioSwim2 spr, spMarioSwim3 spr, spMarioSwim4 spr, spMarioSwim5 spr]

bigSwimSprites :: Sprites -> [Picture]
bigSwimSprites spr = [spBigSwim1 spr, spBigSwim2 spr, spBigSwim3 spr, spBigSwim4 spr, spBigSwim5 spr]

fireSwimSprites :: Sprites -> [Picture]
fireSwimSprites spr = [spFireSwim1 spr, spFireSwim2 spr, spFireSwim3 spr, spFireSwim4 spr, spFireSwim5 spr]

joeSwimSprites :: Sprites -> [Picture]
joeSwimSprites spr = [spJoeSwim1 spr, spJoeSwim2 spr, spJoeSwim3 spr, spJoeSwim4 spr, spJoeSwim5 spr]

pickSmallFrame :: Sprites -> Bool -> Bool -> Bool -> Int -> Picture
pickSmallFrame spr airborne still skidding wFrame
  | airborne    = spMarioJump  spr
  | skidding    = spMarioSkid  spr
  | still       = spMarioStand spr
  | wFrame == 0 = spMarioRun1 spr
  | wFrame == 1 = spMarioRun2 spr
  | otherwise   = spMarioRun3 spr

pickBigFrame :: Sprites -> Bool -> Bool -> Bool -> Bool -> Int -> Picture
pickBigFrame spr airborne still crouching skidding wFrame
  | crouching   = spBigCrouch spr
  | airborne    = spBigJump   spr
  | skidding    = spBigSkid   spr
  | still       = spBigStand  spr
  | wFrame == 0 = spBigRun1   spr
  | wFrame == 1 = spBigRun2   spr
  | otherwise   = spBigRun3   spr

pickFireFrame :: Sprites -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Picture
pickFireFrame spr joe shooting airborne still crouching skidding wFrame
  | joe       = pickJoeFrame  spr shooting airborne still crouching skidding wFrame
  | shooting  = spFireShoot   spr
  | crouching = spFireCrouch  spr
  | airborne  = spFireJump    spr
  | skidding  = spFireSkid    spr
  | still     = spFireStand   spr
  | wFrame == 0 = spFireRun1  spr
  | wFrame == 1 = spFireRun2  spr
  | otherwise   = spFireRun3  spr

pickJoeFrame :: Sprites -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Picture
pickJoeFrame spr shooting airborne still crouching skidding wFrame
  | shooting    = spJoeShoot  spr
  | crouching   = spJoeCrouch spr
  | airborne    = spJoeJump   spr
  | skidding    = spJoeSkid   spr
  | still       = spJoeStand  spr
  | wFrame == 0 = spJoeRun1   spr
  | wFrame == 1 = spJoeRun2   spr
  | otherwise   = spJoeRun3   spr

-- ─── Enemies ──────────────────────────────────────────────────────────────────

isPiranha :: Enemy -> Bool
isPiranha e = eType e == Piranha

drawEnem :: Sprites -> Bool -> Float -> [Enemy] -> Picture
drawEnem spr ug clock = pictures . map (drawE spr ug clock)

drawE :: Sprites -> Bool -> Float -> Enemy -> Picture
drawE spr ug clock e = case eState e of
  EDead _
    | eType e == Piranha -> blank   -- no piranha death sprite
    | otherwise          -> translate cx (eY e + 5) (if ug then spUgGoombaCrushed spr else spGoombaCrushed spr)
  EFallDead _        -> translate cx (eY e + spriteHalf) (drawEnemyBody spr ug clock e)
  EShell timer moving -> translate cx (eY e + spriteHalf) (shellPic timer moving)
  EBowser _ _ _ _     -> translate cx (eY e + spriteHalf) (drawEnemyBody spr ug clock e)
  _               ->
    if shouldDrawAlive e
      then case eType e of
        Piranha -> translate cx (eY e + 36) (drawEnemyBody spr ug clock e)
        Koopa   -> translate cx (eY e + koopaBodyHalf) (drawEnemyBody spr ug clock e)
        _       -> translate cx (eY e + spriteHalf) (drawEnemyBody spr ug clock e)
      else blank
  where
    cx = eX e + ts/2
    -- Goomba sprite: 48px tall, half=24 → spriteHalf=24 is correct.
    -- Koopa body sprite: 72px tall, half=36 → needs its own offset.
    -- Koopa shell sprite: 48px tall, half=24 → same as goomba, correct.
    spriteHalf = 24
    koopaBodyHalf = 36

    shellPic timer moving
      | moving       = if ug then spUgKoopaShell spr     else spKoopaShell spr
      | timer <= 2.0 = if ug then spUgKoopaResetting spr else spKoopaResetting spr
      | otherwise    = if ug then spUgKoopaShell spr     else spKoopaShell spr

    shouldDrawAlive en = case eState en of
      EAlive        -> True
      EPiranha _ up -> up || eY en + 72 > eVY en   -- hide once top of sprite sinks below pipe rim
      _             -> False

drawEnemyBody :: Sprites -> Bool -> Float -> Enemy -> Picture
drawEnemyBody spr ug clock e = case eType e of
  Goomba     -> scale marioScale marioScale $ goombaFrame spr ug clock
  Koopa      -> scale (marioScale * koopaFace e) marioScale $ koopaFrame spr ug clock e
  Piranha    -> drawPiranha spr ug clock
  Bowser     -> drawBowser spr clock e
  CheepCheep   -> scale (marioScale * fishFace e) marioScale $ redCheepFrame   spr clock
  GreenCheep   -> scale (marioScale * fishFace e) marioScale $ greenCheepFrame spr clock
  JumpingCheep -> scale (marioScale * fishFace e) marioScale $ redCheepFrame   spr clock
  Blooper      -> scale marioScale marioScale $ blooperFrame spr clock

goombaFrame :: Sprites -> Bool -> Float -> Picture
goombaFrame spr ug clock =
  if even (floor (clock * 8) :: Int)
    then if ug then spUgGoomba1 spr else spGoomba1 spr
    else if ug then spUgGoomba2 spr else spGoomba2 spr

koopaFrame :: Sprites -> Bool -> Float -> Enemy -> Picture
koopaFrame spr ug clock e = case eState e of
  EShell _ _ -> if ug then spUgKoopaShell spr else spKoopaShell spr
  _ ->
    if even (floor (clock * 8) :: Int)
      then if ug then spUgKoopa1 spr else spKoopa1 spr
      else if ug then spUgKoopa2 spr else spKoopa2 spr

redCheepFrame :: Sprites -> Float -> Picture
redCheepFrame spr clock =
  if even (floor (clock * 8) :: Int)
    then spCheepRed1 spr
    else spCheepRed2 spr

greenCheepFrame :: Sprites -> Float -> Picture
greenCheepFrame spr clock =
  if even (floor (clock * 8) :: Int)
    then spCheepGreen1 spr
    else spCheepGreen2 spr

blooperFrame :: Sprites -> Float -> Picture
blooperFrame spr clock =
  if even (floor (clock * 2) :: Int)
    then spBlooper1 spr
    else spBlooper2 spr

fishFace :: Enemy -> Float
fishFace e = if eVX e >= 0 then 1 else -1

drawBowser :: Sprites -> Float -> Enemy -> Picture
drawBowser spr clock e =
  let frame = (floor (clock * 4) :: Int) `mod` 4
      pic = case frame of
        0 -> spBowser1 spr
        1 -> spBowser2 spr
        2 -> spBowser3 spr
        _ -> spBowser4 spr
      facing = if eVX e > 0 then -1 else 1 :: Float
  in scale (marioScale * facing) marioScale pic

koopaFace :: Enemy -> Float
koopaFace e = if eVX e >= 0 then 1 else -1

-- ─── Primitives ───────────────────────────────────────────────────────────────

drawSkyFor :: GS -> Picture
drawSkyFor gs
  | isUnderground gs = color black (rectangleSolid (fromIntegral sW) (fromIntegral sH))
  | isCastle gs      = color black (rectangleSolid (fromIntegral sW) (fromIntegral sH))
  | otherwise        = color skyBlue (rectangleSolid (fromIntegral sW) (fromIntegral sH))

waterBlue :: Color
waterBlue = makeColorI 66 66 252 255

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

drawTiles :: Sprites -> Bool -> Bool -> Bool -> Float -> Float -> Bool -> [BrickAnim] -> [Tile] -> Picture
drawTiles spr ug uw castle clock flagOff isPipeEntry anims ts_ =
  pictures (map (drawTile spr ug uw castle clock flagOff isPipeEntry anims) ts_)

drawTilesOfType :: Sprites -> Bool -> Bool -> Bool -> Float -> Float -> Bool -> (Tile -> Bool) -> [BrickAnim] -> [Tile] -> Picture
drawTilesOfType spr ug uw castle clock flagOff isPipeEntry p anims ts_ =
  pictures (map (drawTile spr ug uw castle clock flagOff isPipeEntry anims) (filter p ts_))

drawTile :: Sprites -> Bool -> Bool -> Bool -> Float -> Float -> Bool -> [BrickAnim] -> Tile -> Picture
drawTile spr ug uw castle clock flagOff isPipeEntry anims t = translate tx (ty + bump) pic
  where
    tx   = fromIntegral (tCol t) * ts + ts/2
    ty   = fromIntegral (tRow t) * ts + ts/2
    bump = bumpOffset anims t
    castlePic = scale tileScale tileScale (spCastleBrick spr)
    groundPic = if castle    then castlePic
                else if uw   then scale tileScale tileScale (spUwTile spr)
                else if ug   then scale tileScale tileScale (spUgBlockGround spr)
                else              scale tileScale tileScale (spBlockGround spr)
    brickPic  = if castle    then castlePic
                else if ug   then scale tileScale tileScale (spUgBlockBrick spr)
                else              scale tileScale tileScale (spBlockBrick spr)
    stepPic   = if castle    then castlePic
                else if uw   then scale tileScale tileScale (spUwTile spr)
                else if ug   then scale tileScale tileScale (spUgBlockStep spr)
                else              scale tileScale tileScale (spBlockStep spr)
    emptyPic  = if ug then scale tileScale tileScale (spUgBlockHitEmpty spr)
                else       scale tileScale tileScale (spBlockHitEmpty spr)
    pic = case tType t of
      Ground      -> groundPic
      Brick       -> brickPic
      QBlock _    -> scale tileScale tileScale (qBlockFrame spr clock)
      Used        -> emptyPic
      Step        -> stepPic
      -- FirebarTile: single used-? block (pivot only). Castle brick in castle mode.
      FirebarTile -> emptyPic
      Coral       -> blank
      PipeTop     ->
        if isPipeEntry
          then translate (ts/2) (ts * 1.5) (scale 2 2 (spPipe90 spr))
          else
            let h       = fromIntegral (tRow t) :: Float
                scaleX  = 2 * ts / 48
                scaleY  = h * ts / 96
                offsetY = ts * (1 - h) / 2
            in translate (ts/2) offsetY $ scale scaleX scaleY (spPipe spr)
      Pipe        -> blank
      PipeR       -> blank
      FlagPole    -> blank   -- drawn by drawFlagpoles pass
      FlagBase    -> blank   -- drawn by drawFlagpoles pass
      Castle      -> blank   -- drawn by drawOverworldCastles pass
      SlopeLeft   -> groundPic
      SlopeRight  -> groundPic
      Axe         -> if castle then scale (36/48) (40/48) (spCastleAxe spr) else drawAxe

-- | Draw coral tiles (Coral TType) using the coral sprite.
-- Rendered as a separate pass so they can be layered correctly in the world.
drawCoralTiles :: Sprites -> [Tile] -> Picture
drawCoralTiles spr ts_ =
  -- coral.png is 48x140, rendered at tileScale = 32x93px per sprite.
  -- Short columns (<=3 tiles): 1 sprite. Tall columns (>3 tiles): 2 stacked sprites.
  -- Physics tiles cover the full rendered height in both cases.
  pictures
    [ pictures
        [ translate centreX (baseY + rendH / 2 + fromIntegral i * rendH)
                   (scale tileScale tileScale (spCoral spr))
        | i <- [0 .. nSprites - 1]
        ]
    | col <- coralColumns, not (null col)
    , let maxRow   = maximum (map tRow col)
          centreX  = fromIntegral (tCol (head col)) * ts + ts / 2
          baseY    = ts                  -- ground surface
          rendH    = 140 * tileScale     -- 93.3px per sprite
          nSprites = if maxRow <= 3 then 1 else 2
    ]
  where
    coralTiles   = [ t | t <- ts_, tType t == Coral ]
    uniqueCols   = foldr (\c acc -> if c `elem` acc then acc else c:acc) []
                         (map tCol coralTiles)
    coralColumns = [ filter (\t -> tCol t == c) coralTiles | c <- uniqueCols ]

-- | Look up bump offset for a tile (0 if no active BumpAnim for it).
bumpOffset :: [BrickAnim] -> Tile -> Float
bumpOffset anims t = case filter isBump anims of
    (BumpAnim _ _ timeLeft : _) ->
      let progress = 1.0 - (timeLeft / 0.12)
      in sin (pi * progress) * 4.0
    _ -> 0.0
  where
    isBump (BumpAnim c r _) = c == tCol t && r == tRow t
    isBump _                = False

qBlockFrame :: Sprites -> Float -> Picture
qBlockFrame spr clock =
  let frame = (floor (clock * 4) :: Int) `mod` 10
  in case frame of
       8 -> spBlockQuestion3 spr
       9 -> spBlockQuestion3 spr
       f | f < 5 -> spBlockQuestion1 spr
       _          -> spBlockQuestion2 spr

-- ─── Brick / block animations ────────────────────────────────────────────────

drawBrickAnims :: Sprites -> Bool -> Float -> [BrickAnim] -> Picture
drawBrickAnims spr ug clock anims = pictures (map (drawBrickAnim spr ug clock) anims)

drawBrickAnim :: Sprites -> Bool -> Float -> BrickAnim -> Picture
drawBrickAnim _ _ _ (BumpAnim _ _ _) = blank  -- handled by bumpOffset in drawTile

drawBrickAnim spr ug _ (BreakAnim col row timeLeft) =
  let bx = fromIntegral col * ts + ts / 2
      by = fromIntegral row * ts
      broken = if ug then spUgBlockBrickBroken spr else spBlockBrickBroken spr
  in if timeLeft > 0.08
     then translate bx (by + 8) (scale tileScale tileScale broken)
     else
       let age  = 0.08 - timeLeft
           piece vx0 vy0 =
             let px = bx + vx0 * age
                 py = by + 8 + vy0 * age + 0.5 * (-1400) * age * age
             in translate px py $ scale (tileScale * 0.5) (tileScale * 0.5) broken
       in pictures
            [ piece (-120)  300
            , piece   120   300
            , piece  (-80)  180
            , piece    80   180
            ]

drawBrickAnim spr _ clock (CoinPopAnim x y _ _) =
  translate x y (coinFrame spr clock)

drawAxe :: Picture
drawAxe = pictures
  [ color (makeColorI 139 69 19 255) (translate 0 (-8) (rectangleSolid 6 20))
  , color (makeColorI 255 215 0 255) (translate 0 8 (polygon [(-10,0),(10,0),(0,12)]))
  ]

-- | Draw all overworld flagpoles as sprites.
--   flagpole.png is 48×480 px, scaled uniformly by ts/48 = 2/3 → 32×320 px rendered.
--   The pole tiles span rows 1–10.
--     Row 1 bottom edge  = ts/2        =  16
--     Row 10 top edge    = 10*ts+ts/2+ts/2 = 11*ts = 352   (wait: row10 centre=335, top=351)
--   Sprite bottom must align with row-1 bottom (Y=16), sprite top with row-10 top (Y=351).
--   Sprite centre Y = (16 + 351) / 2 = 183.5
--   flag.png (36×36) starts with its centre just below the pole tip; slides down by flagOff.
--   Flag top starts at pole top (Y=351), so flag centre starts at 351-18=333.
--   Flag stops when its bottom reaches the ground surface (Y=32): centre stops at 32+18=50.
drawFlagpoles :: Sprites -> Float -> [Tile] -> Picture
drawFlagpoles spr flagOff tiles = pictures (map drawOnePole poleColumns)
  where
    baseTiles   = [ t | t <- tiles, tType t == FlagBase ]
    poleColumns = [ tCol t | t <- baseTiles ]

    sc = ts / 48   -- uniform scale: 48px wide → 32px

    -- Pole bottom edge sits on the ground surface (top of row-0 tile = ts = 32).
    -- Rendered sprite is 320px tall (480 * 2/3). Centre = 32 + 160 = 192.
    poleBottomY = ts                        -- = 32  (ground surface)
    spriteHalfH = (480 * sc) / 2           -- = 160
    poleCentreY = poleBottomY + spriteHalfH -- = 192

    -- Flag starts at the pole top. Pole top = poleBottomY + 480*sc = 32 + 320 = 352.
    -- Flag centre = poleTop - flagHalf = 352 - 18 = 334. Slides DOWN by flagOff.
    -- Floor: flag bottom at ground surface (Y=32), so flag centre floor = 32 + 18 = 50.
    poleTopY      = poleBottomY + 480 * sc  -- = 352
    flagStartY    = poleTopY - 18           -- = 334 (flag centre at pole top)

    drawOnePole c =
      let poleX    = fromIntegral c * ts + ts / 2
          flagY    = flagStartY - flagOff
      in pictures
           [ translate (poleX + 18) flagY (spFlag spr)
           , translate poleX poleCentreY (scale sc sc (spFlagpole spr))
           ]

-- | Draw overworld end-of-level castles.
--   castle_level_end.png is 240×240 px.
--   mkCastle c produces cols c..c+4 (5 tiles wide) × rows 0..5 (6 rows tall incl battlements).
--   World width  = 5*ts = 160 px  →  scX = 160/240 = 2/3
--   World height = 6*ts = 192 px  →  scY = 192/240 = 4/5
--   Castle bottom must sit ON TOP of the ground surface (Y = ts = 32).
--   Castle centre Y = groundSurface + destH/2 = 32 + 96 = 128.
drawOverworldCastles :: Sprites -> [Tile] -> Picture
drawOverworldCastles spr tiles = pictures (map drawOneCastle castleGroups)
  where
    castleTiles  = [ t | t <- tiles, tType t == Castle ]
    castleGroups = case castleTiles of
      [] -> []
      _  -> [minimum (map tCol castleTiles)]

    sprW  = 240 :: Float
    sprH  = 240 :: Float
    destW = 5 * ts    -- 160
    destH = 6 * ts    -- 192
    scX   = destW / sprW
    scY   = destH / sprH

    -- Ground surface Y = top of row-0 tile = ts/2 + ts/2 = ts = 32
    groundSurface = ts   -- = 32
    castleCentreY = groundSurface + destH / 2   -- = 32 + 96 = 128

    drawOneCastle minC =
      let cx = fromIntegral minC * ts + destW / 2
      in translate cx castleCentreY (scale scX scY (spCastleLevelEnd spr))

drawPiranha :: Sprites -> Bool -> Float -> Picture
drawPiranha spr ug clock =
  if even (floor (clock * 6) :: Int)
    then if ug then spUgPiranha1 spr else spPiranha1 spr
    else if ug then spUgPiranha2 spr else spPiranha2 spr

-- | Draw lava.png (144x72) tiled across each lava pit at natural size.
--   Pits are identified by Ground tiles at row -2.
--   A red fill covers the gap below the sprite to the screen bottom.
--   Floor tiles (drawn after) cover any sprite overflow at the pit edges.
drawLava :: Sprites -> [Tile] -> Picture
drawLava spr tiles = pictures (concatMap drawPit pits)
  where
    sprW  = 144 :: Float
    lavaY = 2 * ts  -- = 64

    lavaRed     = makeColorI 181 50 32 255
    spriteBotY  = lavaY - 36          -- = 28, bottom edge of lava sprite
    fillH       = 600 :: Float        -- tall enough to reach past screen bottom
    fillCentreY = spriteBotY - fillH / 2  -- top of fill at sprite bottom, extends down

    lavaCols = [ tCol t | t <- tiles, tType t == Ground, tRow t == (-2) ]

    -- Group consecutive columns into pits.
    pits = groupConsecutive (foldr insertSorted [] lavaCols)
      where
        insertSorted c []     = [c]
        insertSorted c (x:xs) = if c <= x then c:x:xs else x : insertSorted c xs
        groupConsecutive []     = []
        groupConsecutive (c:cs) = go [c] cs
          where
            go grp []     = [grp]
            go grp (x:xs) = if x == last grp + 1
                              then go (grp ++ [x]) xs
                              else grp : go [x] xs

    drawPit colGroup =
      let c1         = minimum colGroup
          c2         = maximum colGroup
          x1         = fromIntegral c1 * ts
          x2         = fromIntegral (c2 + 1) * ts
          pitW       = x2 - x1
          pitCentreX = (x1 + x2) / 2
          n          = ceiling (pitW / sprW) :: Int
          -- First sprite centred on the pit; additional sprites tiled rightward
          fillRect   = color lavaRed
                         (translate pitCentreX fillCentreY
                            (rectangleSolid pitW fillH))
      in fillRect
       : [ translate (pitCentreX + (fromIntegral i - fromIntegral (n-1) / 2) * sprW) lavaY (spLava spr)
         | i <- [0 .. n - 1] ]

-- | Draw castle_bridge.png (624x64) over the Bowser pit (cols 128-140).
--   Detected by Step tiles at game row 5 in the bridge column range.
--   The bridge sprite is drawn directly — no masking — so the lava beneath
--   it (drawn earlier) remains visible below the bridge planks.
drawCastleBridge :: Sprites -> [Tile] -> Picture
drawCastleBridge spr tiles =
  let bridgeCols = [ tCol t | t <- tiles, tType t == Step, tRow t == 5
                             , tCol t >= 128, tCol t <= 140 ]
  in if null bridgeCols then blank
     else let c1      = minimum bridgeCols
              c2      = maximum bridgeCols
              x1      = fromIntegral c1 * ts
              x2      = fromIntegral (c2 + 1) * ts
              midX    = (x1 + x2) / 2
              -- Bridge sprite: 624x64 px.
              -- Position so its top surface aligns with the floor surface (row 5 top).
              -- Row 5 top in world Y = 5*ts + ts/2 = 176. Sprite centre = 176 - 32 = 144.
              bridgeY = 5 * ts + ts / 2 - 32
          in translate midX bridgeY (spCastleBridge spr)

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

drawPlayerFireballs :: Sprites -> Float -> Bool -> [Fireball] -> Picture
drawPlayerFireballs spr clock joe = pictures . map (drawPlayerFireball spr clock joe)

drawPlayerFireball :: Sprites -> Float -> Bool -> Fireball -> Picture
drawPlayerFireball spr clock joe fb
  | not (fiAlive fb) = blank
  | fiBowser fb      = translate (fiX fb) (fiY fb)
                         $ scale (if fiVX fb > 0 then -1 else 1) 1
                         $ bowserFireFrame spr clock
  | otherwise        = translate (fiX fb) (fiY fb)
                         $ (if joe then joeFireballFrame else fireballFrame) spr clock

joeFireballFrame :: Sprites -> Float -> Picture
joeFireballFrame spr clock =
  let frame = (floor (clock * 8) :: Int) `mod` 4
  in case frame of
       0 -> spJoeFireball1 spr
       1 -> spJoeFireball2 spr
       2 -> spJoeFireball3 spr
       _ -> spJoeFireball4 spr

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

-- ─── Moving Platforms ────────────────────────────────────────────────────────
-- The platform sprite is 120×26 px — the full platform as one image.
-- mpX is the left edge, mpY is the top surface Y in world coords.

drawPlatforms :: Sprites -> [MovingPlatform] -> Picture
drawPlatforms spr = pictures . map (drawPlatform spr)

drawPlatform :: Sprites -> MovingPlatform -> Picture
drawPlatform spr mp =
  -- Centre the 120×26 sprite horizontally over the platform's tile span
  -- and position it so its top surface sits at mpY.
  let platWidthPx = fromIntegral (mpWidth mp) * ts   -- world width in px
      sprW = 120 :: Float
      sprH = 26  :: Float
      cx = mpX mp + platWidthPx / 2                  -- centre X
      cy = mpY mp + ts/2 - sprH/2                    -- centre Y (top flush with mpY+ts/2)
  in translate cx cy (spPlatform spr)

-- ─── Coins ───────────────────────────────────────────────────────────────────
drawCoins :: Sprites -> Bool -> Float -> [(Float,Float,Bool)] -> Picture
drawCoins spr uw clock = pictures . map (drawCoin spr uw clock)

drawCoin :: Sprites -> Bool -> Float -> (Float,Float,Bool) -> Picture
drawCoin _   _  _     (_,_,True) = blank
drawCoin spr _ _ (x,y,_)   =
  translate x y (spCoinExposed spr)

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
      lvlNum    = if lNumber currentLevel == 0 then 2 else lNumber currentLevel
      timerVal  = floor (gTimer gs) :: Int
      -- Timer turns red when below 100 (NES urgency cue)
      timerColor = if timerVal < 100 then red else white
  in pictures
       [ hudLabel (if mJoeMode (gMario gs) then "JOE" else "MARIO") (hudCol 0)
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

-- | Full-screen black intro shown before every level and after every death.
--   Layout (matching original SMB):
--     HUD bar at top (score / world / time)
--     "WORLD X-Y"  centred
--     [sprite]  x  NN   — sprite sits inline to the left of the x
drawLevelIntro :: Sprites -> GS -> Picture
drawLevelIntro spr gs =
  let currentLevel = gLevels gs !! gLevelIdx gs
      joe       = mJoeMode (gMario gs)
      -- The intro cutscene level uses lNumber=0 internally; display "1-2" instead.
      displayNum = if lNumber currentLevel == 0 then 2 else lNumber currentLevel
      worldStr  = "WORLD " ++ show (lWorld currentLevel) ++ "-" ++ show displayNum
      nameStr   = if joe then "JOE" else "MARIO"

      -- Standing sprite matching current power-up and Joe state.
      marioSprite = case mState (gMario gs) of
                      Big  -> spBigStand  spr
                      Fire -> if joe then spJoeStand spr else spFireStand spr
                      _    -> spMarioStand spr

      -- Chunky outlined font: draw in black at 8 surrounding offsets, white on top.
      introScale = 0.20
      outlinePx  = 1.5
      offsets    = [ (dx, dy) | dx <- [-outlinePx, 0, outlinePx]
                               , dy <- [-outlinePx, 0, outlinePx]
                               , (dx, dy) /= (0, 0) ]

      -- Gloss stroke font: ~104 units wide per char at scale 1.0.
      charW     = introScale * 104
      centreX s = -(fromIntegral (length s) * charW / 2)

      introLine s y =
        let cx     = centreX s
            rawTxt = scale introScale introScale (text s)
        in pictures $
             [ translate (cx + dx) (y + dy) (color black rawTxt) | (dx, dy) <- offsets ]
             ++ [ translate cx y (color white rawTxt) ]

      -- ── Inline sprite row: [sprite]  x  NN ─────────────────────────────
      afterSpriteStr = "x  " ++ zeroPad 2 (gLives gs)
      spriteW        = ts
      gapSprTxt      = charW
      afterW         = fromIntegral (length afterSpriteStr) * charW
      rowW           = spriteW + gapSprTxt + afterW
      rowLeft        = -(rowW / 2)

      livesRowY      = -40
      spriteX        = rowLeft + spriteW / 2
      textX          = rowLeft + spriteW + gapSprTxt
      textY          = livesRowY - 10

      livesRow =
        let rawTxt = scale introScale introScale (text afterSpriteStr)
        in pictures $
             [ translate (textX + dx) (textY + dy) (color black rawTxt) | (dx, dy) <- offsets ]
             ++ [ translate textX textY (color white rawTxt)
                , translate spriteX livesRowY marioSprite
                ]

  in pictures
       [ color black (rectangleSolid (fromIntegral sW) (fromIntegral sH))
       , drawHUD gs
       , introLine worldStr 30
       , livesRow
       ]

drawOverlay :: GS -> Picture
drawOverlay gs = case gPhase gs of
  LevelIntro    -> blank   -- handled by drawLevelIntro via the draw short-circuit
  Play          -> blank
  PipeEntry     -> blank
  CastleComplete -> blank
  Over          -> mkOv (dark red)                 "GAME OVER" ("Lives: " ++ show (gLives gs))
  Win           -> mkOv (makeColorI 255 215 0 255) "YOU WIN!"  ("Score: " ++ show (gScore gs))
  LevelComplete -> blank
  where
    currentLevel = gLevels gs !! gLevelIdx gs

    mkOv c t1 t2 = pictures
      [ color (withAlpha 0.65 black) (rectangleSolid 900 700)
      , color c     (translate (-155)   40  (scale 0.45 0.45 (text t1)))
      , color white (translate (-160) (-25) (scale 0.2  0.2  (text t2)))
      , color white (translate (-110) (-65) (scale 0.16 0.16 (text "Press R to restart")))
      ]