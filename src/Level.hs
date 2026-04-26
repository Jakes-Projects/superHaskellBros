module Level (allLevels, initMarioFromLevel) where

import Constants (ts)
import Types

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkRow :: TType -> Int -> Int -> Int -> [Tile]
mkRow t r c1 c2 = [Tile c r t | c <- [c1..c2]]

mkRect :: TType -> Int -> Int -> Int -> Int -> [Tile]
mkRect t c1 c2 r1 r2 = [Tile c r t | c <- [c1..c2], r <- [r1..r2]]

mkGround :: Int -> Int -> [Tile]
mkGround c1 c2 = concatMap (\r -> mkRow Ground r c1 c2) [0, -1, -2, -3, -4]

mkCeiling :: Int -> Int -> Int -> [Tile]
mkCeiling r c1 c2 = mkRow Step r c1 c2

mkPlatform :: Int -> Int -> Int -> [Tile]
mkPlatform r c1 c2 = mkRow Brick r c1 c2

mkQLine :: Int -> Int -> Int -> [Tile]
mkQLine r c1 c2 = mkRow QBlock r c1 c2

mkUsedLine :: Int -> Int -> Int -> [Tile]
mkUsedLine r c1 c2 = mkRow Used r c1 c2

mkPipe :: Int -> Int -> [Tile]
mkPipe c h =
  [Tile c r t | (r,t) <- zip [1..h] (replicate (h-1) Pipe ++ [PipeTop])]
  ++ [Tile (c+1) r PipeR | r <- [1..h]]

mkPipeGroup :: [(Int,Int)] -> [Tile]
mkPipeGroup = concatMap (uncurry mkPipe)

mkStairsUp :: Int -> Int -> [Tile]
mkStairsUp c h =
  concat [ [Tile (c+i) r Step | r <- [1..i+1]]
         | i <- [0..h-1]
         ]

mkStairsDown :: Int -> Int -> [Tile]
mkStairsDown c h =
  concat [ [Tile (c+i) r Step | r <- [1..(h-i)]]
         | i <- [0..h-1]
         ]

mkBridge :: Int -> Int -> [Tile]
mkBridge c1 c2 = mkRow Step 1 c1 c2

mkBridgePosts :: [Int] -> [Tile]
mkBridgePosts cols = [Tile c 0 Step | c <- cols]

mkFlag :: Int -> [Tile]
mkFlag c = [Tile c r FlagPole | r <- [1..10]] ++ [Tile c 0 FlagBase]

mkCastle :: Int -> [Tile]
mkCastle c =
  mkRect Castle c (c+4) 0 4 ++
  [Tile x 5 Castle | x <- [c, c+2, c+4]]

mkCoins :: [(Int,Int)] -> [(Float,Float,Bool)]
mkCoins ps =
  [ (fromIntegral c * ts + ts/2, fromIntegral r * ts + ts/2, False)
  | (c,r) <- ps
  ]

mkLevel
  :: [Tile] -> [Enemy] -> [(Float,Float,Bool)] -> [PUp] -> [Firebar]
  -> Float -> Float -> Float -> Int -> Int -> Level
mkLevel ts_ es cs ps fs sx sy ex w n = Level ts_ es cs ps fs sx sy ex w n

initMarioFromLevel :: Level -> Mario
initMarioFromLevel lvl = Mario (lStartX lvl) (lStartY lvl) 0 0 False Small 1 0 0

--------------------------------------------------------------------------------
-- Enemy helpers
--------------------------------------------------------------------------------

mkG :: Int -> Enemy
mkG c = Enemy (fromIntegral c * ts) ts (-80) 0 EAlive Goomba

mkGAt :: Int -> Int -> Enemy
mkGAt c r = Enemy (fromIntegral c * ts) (fromIntegral r * ts) (-80) 0 EAlive Goomba

mkK :: Int -> Enemy
mkK c = Enemy (fromIntegral c * ts) ts (-70) 0 EAlive Koopa

mkKAt :: Int -> Int -> Enemy
mkKAt c r = Enemy (fromIntegral c * ts) (fromIntegral r * ts) (-70) 0 EAlive Koopa

mkP :: (Int, Int) -> Enemy
mkP (c, r) = Enemy (fromIntegral c * ts) (fromIntegral r * ts) 0 0 (EPiranha 0 False) Piranha

-- | Bowser: 2-tile-wide, spawns at column c.
--   y = ts*2 places him on top of the row-1 bridge tiles (bridge top = 2*ts).
mkBowser :: Int -> Enemy
mkBowser c = Enemy (fromIntegral c * ts) (ts*2) (-60) 0 EAlive Bowser

--------------------------------------------------------------------------------
-- World 1-1
-- Reference: https://www.mariowiki.com/World_1-1
--------------------------------------------------------------------------------

level1_1 :: Level
level1_1 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (204*ts) 1 1
  where
    ground = mkGround 0 211

    blocks =
      -- First brick cluster (cols 20–24, row 3):  B ? B ? B
      -- Original layout: col20=Brick, 21=QBlock, 22=Brick, 23=QBlock, 24=Brick
         mkPlatform 3 20 20
      ++ mkQLine    3 21 21
      ++ mkPlatform 3 22 22
      ++ mkQLine    3 23 23
      ++ mkPlatform 3 24 24
      -- Hidden single QBlock above the gap, col 22, row 7
      ++ mkQLine    7 22 22

      -- Second brick cluster (cols 78–82, row 3): B ? B B B
      -- Row-7 QBlocks: 78, 79, 80
      ++ mkPlatform 3 78 78
      ++ mkQLine    3 79 79
      ++ mkPlatform 3 80 82
      ++ mkQLine    7 78 80

      -- Third cluster (cols 91–95, row 3): B B ? B B
      ++ mkPlatform 3 91 92
      ++ mkQLine    3 93 93
      ++ mkPlatform 3 94 95

      -- Fourth cluster (cols 100–102, row 3): B ? B
      ++ mkPlatform 3 100 100
      ++ mkQLine    3 101 101
      ++ mkPlatform 3 102 102

      -- Fifth cluster (cols 107–110, row 3 & 7):  B B B / B ? B B
      ++ mkPlatform 3 107 109
      ++ mkPlatform 7 107 107
      ++ mkQLine    7 108 108
      ++ mkPlatform 7 109 110

    pipes  = mkPipeGroup [(28,2),(38,3),(46,4),(57,4),(163,2)]
    stairs = mkStairsUp 127 4 ++ mkStairsDown 133 4
          ++ mkStairsUp 140 4 ++ mkStairsDown 146 4
    finish = mkStairsUp 196 8
    flag   = mkFlag 204
    castle = mkCastle 207

    tiles = ground ++ blocks ++ pipes ++ stairs ++ finish ++ flag ++ castle

    enemies =
      -- col 55 instead of 57: pipe (57,4) occupies cols 57-58
      -- col 132 instead of 130: stairsUp 127 fills col 130 rows 1-4;
      --   cols 131-132 are the clear gap between the two staircase pairs
         map mkG [22,37,40,55,59,80,82,100,102,110,116,150,152]
      ++ map mkK [60,92,132]
      ++ map mkP [(28,1),(38,2),(46,3),(57,3),(163,1)]

    -- Coins in the original 1-1 sit *only* inside the ? blocks (col 21, 23,
    -- 79, 82, 93, 101, 108).  They are NOT pre-placed floating in the air;
    -- they fly out when Mario bumps the block.  The only visible coins at
    -- game start are those inside the row-7 hidden blocks.  We keep a small
    -- selection of non-? coins that are genuinely floating in the overworld
    -- (none in the vanilla 1-1, so we leave the coin list empty here and let
    -- bumpBlocks handle the ? block coins).
    coins = mkCoins []

--------------------------------------------------------------------------------
-- World 1-2
-- Image-accurate layout:
--   Above ground  : raised platform (cols 0–17) with Step battlements and
--                   one entry pipe at col 13 (height 2, no Piranha).
--   Underground   : cave ceiling row 11, cols 18–194.
--     Early cave  : "? □ □ □ □" brick row at row 2 (cols 32–36).
--                   Stair A: ascending brick columns heights 1–4 (cols 41–44).
--                   Stair B: same pattern (cols 49–52).
--                   Lone brick col 57.
--     Mid-cave    : 4 coins (row 6) + ? block (row 5, col 73) + 4 coins (row 5).
--     Arch structs: two hollow bracket structures.
--                   Each has a wide top shelf (row 8) with thick legs (rows 5–7)
--                   at each end, leaving an open hollow interior with coins.
--     Right cave  : 6 coins in a row (row 6). Two Piranha pipes.
--   Above-ground  : surface exit pipe → stairs → flag → castle.
--
-- Blocked ground cols (row-1+ tiles — enemies must avoid these):
--   Entry pipe : 13–14     Stair A: 41–44     Stair B: 49–52
--   Lone brick : 57        Warp   : 162–163, 166–167, 170–171
--   Exit pipes : 180–181, 184–185, 196–197    Finish: 200–207
--------------------------------------------------------------------------------

level1_2 :: Level
level1_2 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (208*ts) 1 2
  where
    ground = mkGround 0 215

    -- ── Above-ground intro ────────────────────────────────────────────────
    -- Step battlements at row 3 across the raised platform.
    -- Entry pipe at col 13 (height 2). No Piranha — this is the descent point.
    battlements = mkRow Step 3 0 17
    entryPipe   = mkPipe 13 2

    -- ── Cave ceiling ──────────────────────────────────────────────────────
    -- Starts at col 18 (just past the above-ground cliff) and stops at col 194
    -- so the surface exit pipe at col 196 rises into open air.
    caveCeiling = mkCeiling 11 18 194

    -- ── Early underground ─────────────────────────────────────────────────
    -- "? □ □ □ □": reward block + four bricks at row 2 near the cave entrance.
    questRow = mkQLine 2 32 32 ++ mkPlatform 2 33 36

    -- Ascending brick staircase A (cols 41–44): column i+1 bricks high.
    stairA = concat [ [Tile (41+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]

    -- Ascending brick staircase B (cols 49–52): same pattern.
    stairB = concat [ [Tile (49+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]

    -- Single lone brick (1-tile high) at col 57.
    loneBrick = [Tile 57 1 Brick]

    -- ── Mid-cave coin + reward cluster ────────────────────────────────────
    -- ? block at row 5 col 73 (Mushroom for Small Mario, Fire Flower otherwise).
    midQ = mkQLine 5 73 73

    -- ── Arch / bracket structures ─────────────────────────────────────────
    -- Arch 1 (cols 88–100):
    --   Top shelf : row 8, cols 88–100.
    --   Left leg  : rows 5–7, cols 88–90.
    --   Right leg : rows 5–7, cols 98–100.
    --   Interior  : cols 91–97, rows 5–7 → hollow; coins accessible from below.
    arch1Top  = mkPlatform 8 88 100
    arch1LLeg = concat [ mkPlatform r 88 90  | r <- [5,6,7] ]
    arch1RLeg = concat [ mkPlatform r 98 100 | r <- [5,6,7] ]

    -- Arch 2 (cols 108–120):
    --   Top shelf : row 8, cols 108–120.
    --   Left leg  : rows 5–7, cols 108–110.
    --   Right leg : rows 5–7, cols 118–120.
    --   Interior  : cols 111–117, rows 5–7 → hollow.
    arch2Top  = mkPlatform 8 108 120
    arch2LLeg = concat [ mkPlatform r 108 110 | r <- [5,6,7] ]
    arch2RLeg = concat [ mkPlatform r 118 120 | r <- [5,6,7] ]

    -- ── Warp zone (cols 162–171) ───────────────────────────────────────────
    -- Heights 2 / 3 / 4 → Worlds 2 / 3 / 4.
    warpPipe2 = mkPipe 162 2
    warpPipe3 = mkPipe 166 3
    warpPipe4 = mkPipe 170 4

    -- ── Underground exit pipes (both with Piranha plants) ─────────────────
    underExitA = mkPipe 180 2
    underExitB = mkPipe 184 2

    -- ── Above-ground finish ───────────────────────────────────────────────
    surfaceExit = mkPipe 196 3
    finish      = mkStairsUp 200 8
    flag        = mkFlag 208
    castle      = mkCastle 211

    tiles = ground
         ++ entryPipe ++ caveCeiling
         ++ questRow ++ stairA ++ stairB ++ loneBrick
         ++ midQ
         ++ arch1Top ++ arch1LLeg ++ arch1RLeg
         ++ arch2Top ++ arch2LLeg ++ arch2RLeg
         ++ warpPipe2 ++ warpPipe3 ++ warpPipe4
         ++ underExitA ++ underExitB
         ++ surfaceExit ++ finish ++ flag ++ castle

    -- ── Enemies ───────────────────────────────────────────────────────────
    -- 8 total — matches the sparse enemy count visible in the reference image.
    -- Every ground enemy verified clear of all blocked columns above.
    enemies =
      [ mkG 46    -- just past stairA (stairA ends col 44; col 46 clear)
      , mkG 60    -- after stairB + lone brick (col 57 blocked; 60 clear)
      , mkK 95    -- Koopa in arch1 hollow (cols 91–97 have no row-1 tiles)
      , mkG 115   -- Goomba in arch2 hollow (cols 111–117 clear)
      , mkG 155   -- right cave patrol (col 155 clear; warpPipe2 starts at 162)
      , mkP (180, 1)  -- Piranha in underExitA
      , mkP (184, 1)  -- Piranha in underExitB
      ]

    -- ── Coins ─────────────────────────────────────────────────────────────
    -- Mid-cave: 4 coins at row 6 before midQ; 4 coins at row 5 flanking midQ
    --   (col 73 is occupied by the midQ tile, so those coins skip it).
    -- Arch interiors: 4 coins at row 6 inside each hollow (reachable by jump).
    -- Right cave: 6 coins in a row at row 6.
    coins = mkCoins $
         [(68,6),(69,6),(70,6),(71,6)]                          -- 4 coins before midQ
      ++ [(69,5),(70,5),(71,5),(72,5)]                          -- 4 coins flanking midQ
      ++ [(92,6),(93,6),(94,6),(95,6)]                          -- 4 coins in arch1 hollow
      ++ [(112,6),(113,6),(114,6),(115,6)]                      -- 4 coins in arch2 hollow
      ++ [(140,6),(141,6),(142,6),(143,6),(144,6),(145,6)]      -- 6 coins right cave

--------------------------------------------------------------------------------
-- World 1-3
-- Treetop level: Mario walks across stacked brick platforms (tree canopy).
--------------------------------------------------------------------------------

level1_3 :: Level
level1_3 = mkLevel tiles enemies coins [] [] (ts*3) (ts*5) (244*ts) 1 3
  where
    ground = mkGround 0 244

    islands =
         mkPlatform 3 10 12 ++ mkPlatform 4 10 12
      ++ mkPlatform 3 16 19 ++ mkPlatform 4 16 19
      ++ mkPlatform 5 26 29 ++ mkPlatform 5 30 31
      ++ mkPlatform 4 34 36 ++ mkPlatform 5 34 36
      ++ mkPlatform 3 48 50 ++ mkPlatform 4 48 50
      ++ mkPlatform 4 58 60 ++ mkPlatform 5 58 60
      ++ mkPlatform 7 72 74 ++ mkPlatform 8 72 74
      ++ mkPlatform 5 90 93 ++ mkPlatform 6 90 93
      ++ mkPlatform 4 106 108 ++ mkPlatform 5 106 108
      ++ mkPlatform 6 122 125 ++ mkPlatform 7 122 125
      ++ mkPlatform 4 138 140 ++ mkPlatform 5 138 140
      ++ mkPlatform 7 154 157 ++ mkPlatform 8 154 157
      ++ mkPlatform 4 170 172 ++ mkPlatform 5 170 172
      ++ mkPlatform 6 186 189 ++ mkPlatform 7 186 189
      ++ mkPlatform 4 202 204 ++ mkPlatform 5 202 204
      ++ mkPlatform 7 218 221 ++ mkPlatform 8 218 221
      ++ mkPlatform 4 234 236 ++ mkPlatform 5 234 236

    jumps =
         mkQLine 4 16 17
      ++ mkQLine 6 48 48
      ++ mkQLine 5 80 81
      ++ mkQLine 6 112 112
      ++ mkQLine 5 144 145
      ++ mkQLine 6 176 176
      ++ mkQLine 4 208 209

    tiles = ground ++ islands ++ jumps

    enemies = map mkG [16,18,50,82,114,146,178,210] ++ map mkK [64,128,192]
    coins = mkCoins
      ([(c,4) | c <- [26..29]] ++ [(c,5) | c <- [90..93]] ++ [(c,4) | c <- [122..125]]
       ++ [(c,5) | c <- [154..157]] ++ [(c,4) | c <- [186..189]] ++ [(c,4) | c <- [218..221]]
       ++ [(16,6),(17,6),(48,8),(80,7),(81,7),(112,8),(144,7),(145,7),(176,8),(208,6),(209,6)])

--------------------------------------------------------------------------------
-- World 1-4
-- Bowser's castle.  Firebars, lava pits, and Bowser on the bridge.
-- Bowser is defeated only by touching the Axe at col 77 (already handled by
-- the Axe tile logic in GameState).
--------------------------------------------------------------------------------

level1_4 :: Level
level1_4 = mkLevel tiles enemies coins [] firebars (ts*3) (ts*3) (80*ts) 1 4
  where
    floorA = mkGround 0 15
    floorB = mkGround 20 25
    floorC = mkGround 30 49
    lava   = [Tile c (-2) Ground | c <- [16..19] ++ [26..29]]

    walls  = mkRect Step 0 50 2 10
          ++ [Tile 0 r Step | r <- [0..10]]
          ++ [Tile 50 r Step | r <- [0..10]]

    -- First bridge (over first lava pit and through the main castle hall)
    bridge        = mkBridge 16 39
    bridgeSupport = mkBridgePosts [16,19,22,25,28,31,34,37]

    -- Second bridge leads to Bowser
    secondRun     = mkBridge 40 73
    secondSupport = mkBridgePosts [40,45,50,55,60,65,70,73]

    stairClimb = mkStairsUp 73 6
    axe        = [Tile 77 1 Axe]
    castle     = mkCastle 78

    tiles = floorA ++ floorB ++ floorC ++ lava ++ walls
         ++ bridge ++ bridgeSupport
         ++ secondRun ++ secondSupport
         ++ stairClimb ++ axe ++ castle

    -- Two firebars: one in the first corridor, one in the castle hall
    firebars =
      [ Firebar (34*ts) (3*ts) 0.00 2.4 4
      , Firebar (58*ts) (3*ts) 1.30 2.0 5
      ]

    enemies =
      [ Enemy (10*ts) ts (-80) 0 EAlive Goomba
      , Enemy (12*ts) ts (-80) 0 EAlive Goomba
      , Enemy (25*ts) (ts*2) (-70) 0 EAlive Koopa
      , mkBowser 65   -- Bowser guards the second bridge
      ]

    -- A handful of coins in the first corridor to reward careful play
    coins = mkCoins [(5,2),(6,2),(7,2),(8,2),(9,2),(10,2),(20,2),(25,2),(30,2),(35,2)]

--------------------------------------------------------------------------------
-- All levels exported
--------------------------------------------------------------------------------

allLevels :: [Level]
allLevels = [level1_1, level1_2, level1_3, level1_4]