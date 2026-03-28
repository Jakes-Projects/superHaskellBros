-- Super Mario Bros - World 1-1
-- ─────────────────────────────────────────
-- HOW TO RUN:
--
-- Step 1 — install gloss (once, in a terminal):
--   cabal update && cabal install --lib gloss
--
-- Step 2 — launch GHCi:
--   ghci Mario.hs
--   main
--
-- CONTROLS:
--   Left / Right arrows  (or A / D)  — walk
--   Space  or  Up arrow              — jump
--   Z  or  X                         — run faster
--   R                                 — restart
-- ─────────────────────────────────────────

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- ─────────────────────────────────────────
-- Constants
-- ─────────────────────────────────────────

ts :: Float          -- tile size
ts = 32

grav :: Float
grav = -1400

jumpV :: Float
jumpV = 630

walkSpd, runSpd :: Float
walkSpd = 180
runSpd  = 300

sW, sH :: Int
sW = 800
sH = 600

-- ─────────────────────────────────────────
-- Core types
-- ─────────────────────────────────────────

data TType = Ground | Brick | QBlock | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
  deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa deriving (Eq, Show)

data Enemy = Enemy
  { eX, eY, eVX :: Float
  , eAlive :: Bool
  , eDead  :: Bool      -- squished briefly
  , eTimer :: Float
  , eType  :: EType
  } deriving Show

data PUp = PUp { pX, pY, pVY :: Float, pAlive :: Bool } deriving Show

data MS = Small | Big | MDead deriving (Eq, Show)

data Mario = Mario
  { mX, mY   :: Float
  , mVX, mVY :: Float
  , mGround  :: Bool
  , mState   :: MS
  , mFace    :: Int     -- 1=right -1=left
  , mAnim    :: Float
  , mInv     :: Float   -- invincibility timer
  } deriving Show

data KS = KS { kL, kR, kJ, kRun :: Bool } deriving Show

data Phase = Play | Over | Win deriving (Eq, Show)

data GS = GS
  { gMario :: Mario
  , gTiles :: [Tile]
  , gEnem  :: [Enemy]
  , gPups  :: [PUp]
  , gCoins :: [(Float,Float,Bool)]
  , gScore :: Int
  , gLives :: Int
  , gCam   :: Float
  , gKeys  :: KS
  , gPhase :: Phase
  } deriving Show

-- ─────────────────────────────────────────
-- AABB
-- ─────────────────────────────────────────

type BB = (Float,Float,Float,Float)  -- cx cy w h

hit :: BB -> BB -> Bool
hit (ax,ay,aw,ah) (bx,by,bw,bh) =
  abs(ax-bx) < (aw+bw)/2 && abs(ay-by) < (ah+bh)/2

mBB :: Mario -> BB
mBB m = (mX m, mY m, ts*0.78, if mState m == Big then ts*2 else ts)

tBB :: Tile -> BB
tBB t = (fromIntegral (tCol t)*ts + ts/2,
         fromIntegral (tRow t)*ts + ts/2, ts, ts)

eBB :: Enemy -> BB
eBB e = (eX e + ts/2, eY e + ts/2, ts*0.78, ts*0.78)

solid :: TType -> Bool
solid Ground  = True
solid Brick   = True
solid QBlock  = True
solid Used    = True
solid PipeTop = True
solid Pipe    = True
solid PipeR   = False
solid FlagBase= True
solid Castle  = True
solid _       = False

-- ─────────────────────────────────────────
-- Level Data
-- ─────────────────────────────────────────

mkTiles :: [Tile]
mkTiles = ground ++ structs ++ pipes ++ stairs ++ flag ++ castle
  where
    -- Ground: two rows deep (visible top row + subterranean row)
    ground = [Tile c r Ground | c <- [0..211], r <- [0,(-1)]]

    -- Precisely mapped from the SMB1-1 reference image
    structs =
      -- Area 1: single ? block (mushroom)
      [ Tile 16 3 QBlock
      -- Row of B?BB?B at row 3, then high ? at row 7
      , Tile 20 3 Brick
      , Tile 21 3 QBlock   -- coin
      , Tile 22 3 Brick
      , Tile 23 3 QBlock   -- coin
      , Tile 24 3 Brick
      , Tile 22 7 QBlock   -- secret high ? (star)
      -- Area 2: after pipes - brick+? row at row 3
      , Tile 78 3 Brick
      , Tile 79 3 QBlock   -- coin
      , Tile 80 3 Brick
      , Tile 81 3 Brick
      , Tile 82 3 QBlock   -- coin
      , Tile 83 3 Brick
      -- High row at row 7
      , Tile 78 7 QBlock
      , Tile 79 7 QBlock
      , Tile 80 7 QBlock
      , Tile 81 7 Brick
      -- Gap area platforms
      , Tile 91 3 Brick
      , Tile 92 3 Brick
      , Tile 93 3 QBlock
      , Tile 94 3 Brick
      , Tile 95 3 Brick
      -- Second gap area
      , Tile 100 3 Brick
      , Tile 101 3 QBlock
      , Tile 102 3 Brick
      -- Late area bricks
      , Tile 107 3 Brick
      , Tile 108 3 Brick
      , Tile 109 3 Brick
      , Tile 107 7 Brick
      , Tile 108 7 QBlock
      , Tile 109 7 Brick
      , Tile 110 7 Brick
      ]

    -- Pipes: each pipe is 2 cols wide (left col listed), PipeTop on the top row
    pipes = concatMap mkPipe
      [ (28, 2)   -- pipe 1: 2 tall
      , (38, 3)   -- pipe 2: 3 tall
      , (46, 4)   -- pipe 3: 4 tall
      , (57, 4)   -- pipe 4: 4 tall
      , (163, 2)  -- pipe 5 (after second stair set): 2 tall
      ]

    mkPipe (col, height) =
      [Tile col r t | (r,t) <- zip [1..height] (replicate (height-1) Pipe ++ [PipeTop])]
      ++ [Tile (col+1) r PipeR | r <- [1..height]]

    -- Stair pyramids (ascending then descending pairs)
    stairs =
      stairU 127 4 ++ stairD 133 4 ++   -- first pair
      stairU 140 4 ++ stairD 146 4 ++   -- second pair
      -- final big staircase up to flag (8 steps)
      [Tile (196+i) r Ground | i <- [0..7], r <- [1..i+1]]

    stairU c h = [Tile (c+i) r Ground | i <- [0..h-1], r <- [1..i+1]]
    stairD c h = [Tile (c+i) r Ground | i <- [0..h-1], r <- [1..(h-i)]]

    -- Flag pole
    flag = [Tile 204 r FlagPole | r <- [1..10]] ++ [Tile 204 0 FlagBase]

    -- Castle (cols 207-211, rows 0-5 approx)
    castle =
      -- base rows
      [ Tile c r Castle | c <- [207..211], r <- [0..4] ]
      -- battlements (top row with gaps)
      ++ [ Tile c 5 Castle | c <- [207,209,211] ]
      -- door (empty at 208-209 rows 0-1, we just leave those as no-tile)

mkEnemies :: [Enemy]
mkEnemies = map mkG gPs ++ map mkK kPs
  where
    mkG c = Enemy (c*ts) ts (-80) True False 0 Goomba
    mkK c = Enemy (c*ts) ts (-70) True False 0 Koopa
    -- Goombas from map image: pairs at start, after pipes, mid-level, late
    gPs = [20, 22,            -- first two near start
           37, 40,            -- between pipe 1 and 2
           57, 59,            -- near pipe 3 entrance
           80, 82,            -- on brick platform area
           100, 102,          -- mid gap area
           110, 116,          -- late ground
           150, 152]          -- near final stairs
    kPs = [60, 92, 130]       -- Koopas: post pipe4, mid, late

mkCoins :: [(Float,Float,Bool)]
mkCoins =
  [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False)
  | (c,r) <-
    -- Row of coins at start (just above ground)
    [(c,2) | c <- [1..4]]
    -- Coins above ? blocks (float above block row)
    ++ [(21,5),(23,5)          -- above first ? group
       ,(79,5),(82,5)          -- above second ? group
       ,(93,5),(101,5)         -- mid level
       ,(108,9)                -- above high ? block
       ]]

-- ─────────────────────────────────────────
-- Initial State
-- ─────────────────────────────────────────

initMario :: Mario
initMario = Mario (ts*3) (ts*1.5) 0 0 False Small 1 0 0

initKS :: KS
initKS = KS False False False False

initGS :: GS
initGS = GS initMario mkTiles mkEnemies [] mkCoins 0 3 0 initKS Play

-- ─────────────────────────────────────────
-- Update
-- ─────────────────────────────────────────

step :: Float -> GS -> GS
step dt gs
  | gPhase gs /= Play = gs
  | otherwise = gs'
  where
    ks  = gKeys gs
    sol = filter (solid . tType) (gTiles gs)

    m0  = inputMario ks (gMario gs)
    -- dead mario just flies up then falls, no tile collision
    m1  = if mState m0 == MDead
            then m0 { mVY = max (-900) (mVY m0 + grav * dt)
                    , mY  = mY m0 + mVY m0 * dt }
            else physicsMario dt sol m0
    m4  = m1 { mAnim = mAnim m1 + dt
              , mInv = max 0 (mInv m1 - dt) }

    cam = max (gCam gs) (mX m4 - fromIntegral sW * 0.35)

    es1 = map (stepEnemy dt sol) (gEnem gs)
    es2 = filter (\e -> eAlive e || eTimer e > 0) es1

    (m5, es3, sc1) = collideEnemies m4 es2 (gScore gs)
    (cs, sc2)      = pickCoins (mBB m5) (gCoins gs) sc1
    (ts2, pu1, sc3)= bumpBlocks m5 (mVY m0) (gTiles gs) (gPups gs) sc2
    pu2            = map (stepPup dt (filter (solid . tType) ts2)) pu1
    (m6, pu3, sc4) = grabPups m5 pu2 sc3

    (m7, ph) = deathCheck m6 (gLives gs)
    ph2 | ph == Play && mX m6 >= 204*ts = Win
        | otherwise = ph

    gs' = gs { gMario = m7
             , gTiles = ts2
             , gEnem  = es3
             , gPups  = pu3
             , gCoins = cs
             , gScore = sc4
             , gLives = if ph /= Play then max 0 (gLives gs - 1) else gLives gs
             , gCam   = cam
             , gPhase = ph2 }

inputMario :: KS -> Mario -> Mario
inputMario ks m
  | mState m == MDead = m
  | otherwise = m { mVX = vx, mFace = f }
  where
    spd = if kRun ks then runSpd else walkSpd
    vx | kL ks    = -spd
       | kR ks    =  spd
       | otherwise = mVX m * 0.78
    f  | kL ks    = -1
       | kR ks    =  1
       | otherwise = mFace m

-- Physics: sub-step sweep. Move in small increments so we never tunnel through a tile.
-- This is the simplest approach that actually works correctly.

physicsMario :: Float -> [Tile] -> Mario -> Mario
physicsMario dt sol m = m { mX = nx, mY = ny, mVX = nvx, mVY = nvy, mGround = grounded }
  where
    (_, _, cw, ch) = mBB m
    halfW = cw * 0.45
    halfH = ch * 0.5

    -- tile left/right/top/bottom edges
    tLeft  t = fromIntegral (tCol t) * ts
    tRight t = tLeft t + ts
    tBot   t = fromIntegral (tRow t) * ts
    tTop   t = tBot t + ts

    -- Mario's edges given a centre position
    mLeft  x = x - halfW
    mRight x = x + halfW
    mBottom y = y - halfH
    mTop    y = y + halfH

    -- overlap test on edges (not centres)
    overlapXY x y t =
      mRight x > tLeft t && mLeft x < tRight t &&
      mTop   y > tBot   t && mBottom y < tTop  t

    -- 1. Apply gravity
    vy1 = max (-900) (mVY m + grav * dt)

    -- 2. Move X, resolve X collisions by pushing Mario out horizontally
    x1 = mX m + mVX m * dt
    x1c = max halfW x1   -- don't go past left edge of world
    xCols = filter (overlapXY x1c (mY m)) sol
    (nx, nvx)
      | null xCols = (x1c, mVX m)
      | mVX m > 0  = -- moving right, push left
          let blocking = minimum (map tLeft xCols)
          in (blocking - halfW - 0.5, 0)
      | mVX m < 0  = -- moving left, push right
          let blocking = maximum (map tRight xCols)
          in (blocking + halfW + 0.5, 0)
      | otherwise  = (mX m, 0)

    -- 3. Move Y, resolve Y collisions by pushing Mario out vertically
    y1 = mY m + vy1 * dt
    yCols = filter (overlapXY nx y1) sol
    (ny, nvy, grounded)
      | null yCols = (y1, vy1, False)
      | vy1 <= 0 =  -- falling or resting, land on top of tile
          let blocking = maximum (map tTop yCols)
          in (blocking + halfH, 0, True)
      | otherwise =  -- jumping, hit ceiling
          let blocking = minimum (map tBot yCols)
          in (blocking - halfH, -50, False)


stepEnemy :: Float -> [Tile] -> Enemy -> Enemy
stepEnemy dt sol e
  | not (eAlive e) = e { eTimer = eTimer e - dt }
  | otherwise = e { eX = ex', eVX = vx', eY = ey' }
  where
    ex0  = eX e + eVX e * dt
    wall = any (hit (ex0+ts/2, eY e+ts/2, ts*0.7, ts*0.7) . tBB) sol
    vx'  = if wall then -(eVX e) else eVX e
    ex'  = if wall then eX e else max 0 ex0
    ey0  = eY e - 60*dt
    onG  = any (hit (ex'+ts/2, ey0+ts/2, ts*0.5, ts*0.5) . tBB) sol
    ey'  = if onG then eY e else ey0

collideEnemies :: Mario -> [Enemy] -> Int -> (Mario,[Enemy],Int)
collideEnemies m es sc = foldr go (m,[],sc) es
  where
    go e (mario,acc,s)
      | not (eAlive e)                         = (mario, e:acc, s)
      | mInv mario > 0                         = (mario, e:acc, s)
      | not (hit (mBB mario) (eBB e))          = (mario, e:acc, s)
      | mY mario > eY e + ts*0.55, mVY mario < 50 =
          (mario { mVY=340 }, e { eAlive=False, eDead=True, eTimer=0.5 }:acc, s+100)
      | mState mario == Big =
          (mario { mState=Small, mInv=2 }, e:acc, s)
      | otherwise =
          (mario { mState=MDead, mVY=500, mVX=0 }, e:acc, s)

pickCoins :: BB -> [(Float,Float,Bool)] -> Int -> ([(Float,Float,Bool)],Int)
pickCoins mb cs sc = foldr go ([],sc) cs
  where
    go (x,y,True)  (acc,s) = ((x,y,True):acc, s)
    go (x,y,False) (acc,s)
      | hit mb (x,y,ts*0.5,ts*0.5) = ((x,y,True):acc, s+200)
      | otherwise                   = ((x,y,False):acc, s)

bumpBlocks :: Mario -> Float -> [Tile] -> [PUp] -> Int -> ([Tile],[PUp],Int)
bumpBlocks m vy tls pus sc
  | vy <= 0   = (tls, pus, sc)
  | otherwise = (tls', pus', sc')
  where
    (_,my,mw,mh) = mBB m
    headB = (mX m, my + mh/2 + 2, mw*0.65, 6)
    -- find the ONE tile Mario's head is hitting (first match only)
    bumped = filter (hit headB . tBB) tls
    (tls', pus', sc') = case bumped of
      []    -> (tls, pus, sc)
      (t:_) -> case tType t of
        QBlock ->
          let tls2 = map (\x -> if samePos x t then x { tType = Used } else x) tls
              pu0  = PUp (fromIntegral (tCol t) * ts)
                         (fromIntegral (tRow t + 1) * ts + ts * 0.5)
                         120 True
          in (tls2, pu0:pus, sc + 50)
        Brick | mState m == Big ->
          let tls2 = filter (\x -> not (samePos x t)) tls
          in (tls2, pus, sc + 50)
        _ -> (tls, pus, sc)
    samePos a b = tCol a == tCol b && tRow a == tRow b

stepPup :: Float -> [Tile] -> PUp -> PUp
stepPup dt sol p
  | not (pAlive p) = p
  | otherwise = p { pX=x', pY=y', pVY=vy' }
  where
    x0  = pX p + 80*dt
    y0  = pY p + pVY p * dt
    vy0 = pVY p + grav*dt
    onG = any (hit (x0+ts/2, y0, ts*0.8, ts*0.8) . tBB) sol
    x'  = x0
    y'  = if onG then pY p else y0
    vy' = if onG then 0 else vy0

grabPups :: Mario -> [PUp] -> Int -> (Mario,[PUp],Int)
grabPups m ps sc = foldr go (m,[],sc) ps
  where
    go p (mario,acc,s)
      | not (pAlive p) = (mario, p:acc, s)
      | hit (mBB mario) (pX p+ts/2, pY p, ts*0.85, ts*0.85) =
          (mario { mState=Big }, p { pAlive=False }:acc, s+1000)
      | otherwise = (mario, p:acc, s)
    hit (ax,ay,aw,ah) (bx,by,bw,bh) =
      abs(ax-bx)<(aw+bw)/2 && abs(ay-by)<(ah+bh)/2

deathCheck :: Mario -> Int -> (Mario, Phase)
deathCheck m lv
  | mY m < -300 = (initMario, if lv <= 1 then Over else Play)
  | otherwise   = (m, Play)

-- ─────────────────────────────────────────
-- Input
-- ─────────────────────────────────────────

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv _ gs | gPhase gs /= Play = gs
handleEv ev gs = case ev of
  EventKey k Down _ _ -> gs { gMario = tryJump k (gMario gs)
                             , gKeys  = setK k True  (gKeys gs) }
  EventKey k Up   _ _ -> gs { gKeys  = setK k False (gKeys gs) }
  _ -> gs
  where
    setK (Char 'a')             v k = k { kL   = v }
    setK (Char 'd')             v k = k { kR   = v }
    setK (SpecialKey KeyLeft)   v k = k { kL   = v }
    setK (SpecialKey KeyRight)  v k = k { kR   = v }
    setK (SpecialKey KeySpace)  v k = k { kJ   = v }
    setK (SpecialKey KeyUp)     v k = k { kJ   = v }
    setK (Char 'z')             v k = k { kRun = v }
    setK (Char 'x')             v k = k { kRun = v }
    setK _ _ k = k

    tryJump (SpecialKey KeySpace) m = doJump m
    tryJump (SpecialKey KeyUp)    m = doJump m
    tryJump _ m = m

    doJump m | mGround m && mState m /= MDead = m { mVY = jumpV, mGround = False }
             | otherwise = m

-- ─────────────────────────────────────────
-- Rendering
-- ─────────────────────────────────────────

-- SMB colour palette
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
      [ drawDecorations       -- clouds, hills, bushes (behind everything)
      , drawTiles (gTiles gs)
      , drawCoins (gCoins gs)
      , drawPups  (gPups  gs)
      , drawEnem  (gEnem  gs)
      , drawMario (gMario gs)
      ]

drawSky :: Picture
drawSky = color skyBlue (rectangleSolid (fromIntegral sW) (fromIntegral sH))

-- ── Decorations (clouds, hills, bushes) ────────────────────────────

ellipseS :: Float -> Float -> Picture
ellipseS rx ry = scale rx ry (circleSolid 1)

drawDecorations :: Picture
drawDecorations = pictures $
  map drawCloud cloudPositions ++
  map drawHill  hillPositions  ++
  map drawBush  bushPositions

-- (col, row, size) -- size 1=small 2=big
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

drawCloud :: (Float,Float,Int) -> Picture
drawCloud (c,_r,sz) =
  let x = c * ts + ts/2
      y = 10 * ts   -- row 10 in world coordinates
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

-- ── Tiles ──────────────────────────────

drawTiles :: [Tile] -> Picture
drawTiles ts_ = pictures (map drawTile (filter visRow ts_))
  where visRow t = tRow t >= 0   -- skip underground row

drawTile :: Tile -> Picture
drawTile t = translate tx ty pic
  where
    tx  = fromIntegral (tCol t)*ts + ts/2
    ty  = fromIntegral (tRow t)*ts + ts/2
    pic = case tType t of
      Ground  -> drawGround
      Brick   -> drawBrick
      QBlock  -> drawQBlock
      Used    -> drawUsed
      PipeTop -> drawPipeTop
      Pipe    -> drawPipe
      PipeR   -> blank
      FlagPole-> drawFlagPole
      FlagBase-> drawFlagBase
      Castle  -> drawCastle t
      _       -> blank

-- Ground tile: brick-pattern with green top strip
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

-- Brick tile
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

-- Question block
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

-- Pipe: drawn as a 2-wide unit; left col draws the full pipe, right col is blank
drawPipeTop :: Picture
drawPipeTop = pictures
  -- Wide lip
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
      (translate 6 (ts*0.3) (polygon [(-2,-8),(-2,8),(12,0)]))  -- flag
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

brickPat :: Picture
brickPat = pictures
  [ line [(-13,0),(13,0)], line [(0,-13),(0,13)]
  , line [(-13,0),(-13,13)], line [(13,0),(13,-13)] ]

-- ── Mario sprite ──────────────────────
-- Pixel grid: 0=transparent 1=red(hat/shirt) 2=skin 3=brown(hair/shoes)
-- 4=darkbrown(outline) 5=orange(overalls)
-- Each row is 12 pixels wide, 16 rows tall, pixel = 2x2 units

marioPixels :: [[Int]]
marioPixels =
  [ [0,0,0,1,1,1,1,1,0,0,0,0]  -- row 15 (top)
  , [0,0,1,1,1,1,1,1,1,1,0,0]
  , [0,0,3,3,3,2,2,3,2,0,0,0]
  , [0,3,2,3,2,2,2,3,2,2,2,0]
  , [0,3,2,3,3,2,2,2,3,2,2,2]  -- row 11
  , [0,3,3,2,2,2,2,3,3,3,3,0]
  , [0,0,0,2,2,2,2,2,2,2,0,0]
  , [0,0,5,5,1,5,5,5,1,5,0,0]  -- row 8: overalls start
  , [0,5,5,5,1,5,5,5,1,5,5,0]
  , [5,5,5,5,1,1,1,1,1,5,5,5]
  , [2,2,5,5,5,5,5,5,5,5,2,2]  -- row 5
  , [2,2,2,5,5,5,5,5,5,2,2,2]
  , [0,0,2,2,2,0,0,2,2,2,0,0]
  , [0,3,3,3,0,0,0,0,3,3,3,0]
  , [3,3,3,3,0,0,0,0,3,3,3,3]  -- row 1
  , [3,3,3,0,0,0,0,0,0,3,3,3]  -- row 0 (bottom)
  ]

pixelColor :: Int -> Maybe Color
pixelColor 1 = Just (makeColorI 210 40  20  255)  -- red
pixelColor 2 = Just (makeColorI 240 185 120 255)  -- skin
pixelColor 3 = Just (makeColorI 100 60  10  255)  -- brown
pixelColor 4 = Just (makeColorI 60  30  0   255)  -- dark
pixelColor 5 = Just (makeColorI 200 70  20  255)  -- orange/overalls
pixelColor _ = Nothing

drawSpriteAt :: Float -> Float -> Float -> Int -> Picture
drawSpriteAt px py psz face =
  let rows   = length marioPixels
      cols   = length (head marioPixels)
      -- centre the sprite
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

-- ── Enemies ────────────────────────────

drawEnem :: [Enemy] -> Picture
drawEnem = pictures . map drawE

drawE :: Enemy -> Picture
drawE e
  | eDead e        = translate cx (eY e + 5) flat
  | not (eAlive e) = blank
  | otherwise      = translate cx (eY e + ts/2) body
  where
    cx   = eX e + ts/2
    flat = color (makeColorI 130 70 15 255) (rectangleSolid (ts*0.9) 9)
    body = case eType e of
      Goomba -> pictures
        [ color (makeColorI 130 70 15 255)  (rectangleSolid (ts*0.85) (ts*0.8))
        , color (makeColorI 70 35 0 255)    (translate 0 (-ts*0.28) (rectangleSolid (ts*0.9) (ts*0.22)))
        , color white  (translate (-7) 5 (circleSolid 5))
        , color white  (translate  (7) 5 (circleSolid 5))
        , color black  (translate (-7) 5 (circleSolid 2.5))
        , color black  (translate  (7) 5 (circleSolid 2.5))
        ]
      Koopa -> pictures
        [ color (makeColorI 0 175 0 255)    (rectangleSolid (ts*0.8) (ts*1.05))
        , color (makeColorI 255 215 90 255) (translate 0 (ts*0.32) (circleSolid (ts*0.28)))
        , color (makeColorI 0 120 0 255)    (scale (ts*0.32) (ts*0.48) (circleSolid 1))
        , color white  (translate (-6) (ts*0.28) (circleSolid 4))
        , color white  (translate  (6) (ts*0.28) (circleSolid 4))
        , color black  (translate (-6) (ts*0.28) (circleSolid 2))
        , color black  (translate  (6) (ts*0.28) (circleSolid 2))
        ]

-- ── Power-ups ──────────────────────────

drawPups :: [PUp] -> Picture
drawPups = pictures . map drawPup

drawPup :: PUp -> Picture
drawPup p
  | not (pAlive p) = blank
  | otherwise = translate (pX p + ts/2) (pY p) $
      pictures [ color (makeColorI 220 50 50 255) (rectangleSolid (ts*0.85) (ts*0.85))
               , color white (translate (-4) 2 (scale 0.22 0.22 (text "M"))) ]

-- ── Coins ──────────────────────────────

drawCoins :: [(Float,Float,Bool)] -> Picture
drawCoins = pictures . map drawCoin

drawCoin :: (Float,Float,Bool) -> Picture
drawCoin (_,_,True) = blank
drawCoin (x,y,_)    = translate x y $
  pictures [ color (makeColorI 255 215 0 255) (circleSolid 9)
           , color (makeColorI 255 240 60 255) (circleSolid 5.5) ]

-- ── HUD ────────────────────────────────

drawHUD :: GS -> Picture
drawHUD gs = translate (-370) 262 $ pictures
  [ color white (scale 0.14 0.14 (text ("SCORE " ++ show (gScore gs))))
  , translate 0 (-22) $ color white (scale 0.14 0.14 (text ("LIVES  " ++ show (gLives gs))))
  , translate 250 0   $ color white (scale 0.14 0.14 (text "WORLD 1-1"))
  ]

-- ── Overlay ────────────────────────────

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

-- ─────────────────────────────────────────
-- Main
-- ─────────────────────────────────────────

win :: Display
win = InWindow "Super Mario Bros — World 1-1" (sW, sH) (80,80)

bgCol :: Color
bgCol = makeColorI 92 148 252 255

main :: IO ()
main = play win bgCol 60 initGS draw handleEv step
