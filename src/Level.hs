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
-- World 2-1
-- Overworld: denser than 1-1 — more enemies, tighter pipe placement,
-- multi-row block clusters, and longer stretches between safe ground.
--------------------------------------------------------------------------------

level2_1 :: Level
level2_1 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (204*ts) 2 1
  where
    ground = mkGround 0 212

    blocks =
      -- Cluster 1 (row 3, cols 28-30): B ? B
         mkPlatform 3 28 28
      ++ mkQLine    3 29 29
      ++ mkPlatform 3 30 30
      -- Cluster 2 (row 3, cols 46-50): B ? B ? B  and row-7 triple
      ++ mkPlatform 3 46 46
      ++ mkQLine    3 47 47
      ++ mkPlatform 3 48 48
      ++ mkQLine    3 49 49
      ++ mkPlatform 3 50 50
      ++ mkQLine    7 47 49
      -- Cluster 3 (row 3, cols 83-87): B B ? B B
      ++ mkPlatform 3 83 84
      ++ mkQLine    3 85 85
      ++ mkPlatform 3 86 87
      -- Cluster 4 (row 3, cols 100-102): B ? B
      ++ mkPlatform 3 100 100
      ++ mkQLine    3 101 101
      ++ mkPlatform 3 102 102
      -- Cluster 5 (rows 3+7, cols 108-111): B B B B / ? B ? B
      ++ mkPlatform 3 108 111
      ++ mkQLine    7 108 108
      ++ mkPlatform 7 109 109
      ++ mkQLine    7 110 110
      ++ mkPlatform 7 111 111
      -- Row-3 solo blocks in the back half
      ++ mkQLine    3 130 130
      ++ mkQLine    3 133 133
      ++ mkPlatform 3 134 136
      ++ mkQLine    3 160 162
      ++ mkPlatform 3 163 165

    pipes  = mkPipeGroup [(22,2),(40,3),(55,3),(70,4),(90,2),(150,2),(165,3)]
    stairs = mkStairsUp 188 4 ++ mkStairsDown 194 4
    finish = mkStairsUp 196 8
    flag   = mkFlag 204
    castle = mkCastle 207

    tiles = ground ++ blocks ++ pipes ++ stairs ++ finish ++ flag ++ castle

    enemies =
         map mkG [10,28,45,62,80,97,112,126,140,155,170,183,193]
      ++ map mkK [35,60,86,106,118,158]
      ++ map mkP [(40,2),(70,3),(165,2)]

    coins = mkCoins $
         [(47,5),(48,5),(49,5)]       -- above row-3 cluster 2
      ++ [(85,5)]                     -- above cluster 3 ? block
      ++ [(101,5)]                    -- above cluster 4 ? block
      ++ [(108,9),(109,9),(110,9)]    -- above row-7 cluster 5
      ++ [(130,5),(133,5)]            -- solo Q blocks
      ++ [(160,5),(161,5),(162,5)]    -- back-half row

--------------------------------------------------------------------------------
-- World 2-2
-- NOTE: The original 2-2 is an underwater level with Bloopers and Cheep-cheeps.
-- Swimming physics and those enemy types are not yet implemented, so this is a
-- placeholder underground level with a different layout from 1-2.
-- The ceiling is slightly lower, platforms form an S-curve, and enemies are
-- denser to reflect the increased difficulty of World 2.
--------------------------------------------------------------------------------

level2_2 :: Level
level2_2 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (208*ts) 2 2
  where
    ground = mkGround 0 215

    entryPipe = mkPipe 14 2

    -- Tighter ceiling than 1-2 (row 10 instead of 11)
    caveCeiling = mkCeiling 10 18 192

    -- Early cave: ? block row + two ascending stair formations
    questRow = mkQLine 2 30 30 ++ mkPlatform 2 31 34
    stairA   = concat [ [Tile (42+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]
    stairB   = concat [ [Tile (50+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]

    -- Mid-cave: offset platform pair + ? blocks
    midShelfL  = mkPlatform 4 62 68 ++ mkPlatform 4 72 76
    midQBlocks = mkQLine 4 69 71
    highShelf  = mkPlatform 7 80 92
    highQ      = mkQLine 7 86 87

    -- Right cave: S-curve formations
    lowerPlat  = mkPlatform 4 98 112
    lowerQ     = mkQLine 4 105 106
    upperPlat  = mkPlatform 7 115 130
    upperQ     = mkQLine 7 122 123
    finalPlat  = mkPlatform 4 133 148

    -- Warp zone
    warpPipe2  = mkPipe 162 2
    warpPipe3  = mkPipe 166 3
    warpPipe4  = mkPipe 170 4

    -- Two exit pipes with Piranhas
    underExitA = mkPipe 185 2
    underExitB = mkPipe 189 2

    surfaceExit = mkPipe 194 3
    finish      = mkStairsUp 200 8
    flag        = mkFlag 208
    castle      = mkCastle 211

    tiles = ground
         ++ entryPipe ++ caveCeiling
         ++ questRow ++ stairA ++ stairB
         ++ midShelfL ++ midQBlocks
         ++ highShelf ++ highQ
         ++ lowerPlat ++ lowerQ
         ++ upperPlat ++ upperQ
         ++ finalPlat
         ++ warpPipe2 ++ warpPipe3 ++ warpPipe4
         ++ underExitA ++ underExitB
         ++ surfaceExit ++ finish ++ flag ++ castle

    enemies =
         map mkG [35,56,70,85,100,118,130,148,175,195]
      ++ map mkK [48,78,110,140]
      ++ [ mkP (185,1), mkP (189,1) ]

    coins = mkCoins $
         [(30,4),(31,4),(32,4)]          -- early ? row
      ++ [(69,6),(70,6),(71,6)]          -- above midShelfL Q blocks
      ++ [(86,9),(87,9)]                 -- above highShelf Q blocks
      ++ [(105,6),(106,6)]               -- above lowerPlat Q blocks
      ++ [(122,9),(123,9)]               -- above upperPlat Q blocks

--------------------------------------------------------------------------------
-- World 2-3
-- Overworld with pits: ground is broken into segments with gaps.
-- Mario must jump the gaps; platforms bridge the wider ones.
-- Original 2-3 has Hammer Bros; substituted with Koopas.
--------------------------------------------------------------------------------

level2_3 :: Level
level2_3 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (213*ts) 2 3
  where
    -- Broken ground segments (gaps between segments are instant death)
    seg1  = mkGround 0   16
    seg2  = mkGround 19  36
    seg3  = mkGround 39  57
    seg4  = mkGround 60  80
    seg5  = mkGround 83  110
    seg6  = mkGround 113 135
    seg7  = mkGround 138 175
    seg8  = mkGround 178 220
    ground = seg1 ++ seg2 ++ seg3 ++ seg4 ++ seg5 ++ seg6 ++ seg7 ++ seg8

    -- Platforms bridging the gaps (also act as collectible routes)
    plat1 = mkPlatform 3 14 21   -- over gap 17-18
    plat2 = mkPlatform 4 37 44   -- over gap 37-38
    plat3 = mkPlatform 3 55 62   -- over gap 58-59
    plat4 = mkPlatform 5 78 86   -- over gap 81-82
    plat5 = mkPlatform 3 108 116 -- over gap 111-112
    plat6 = mkPlatform 4 133 140 -- over gap 136-137
    plat7 = mkPlatform 3 173 180 -- over gap 176-177

    -- ? blocks sprinkled above the platforms
    qBlocks =
         mkQLine 5 15 16
      ++ mkQLine 6 38 40
      ++ mkQLine 5 56 57
      ++ mkQLine 7 79 82
      ++ mkQLine 5 109 112
      ++ mkQLine 6 134 136
      ++ mkQLine 5 174 176

    -- Pipes at the edges of some segments (with Piranhas)
    pipes = mkPipeGroup [(16,2),(57,2),(110,3),(175,2)]

    finish = mkStairsUp 205 8
    flag   = mkFlag 213
    castle = mkCastle 216

    tiles = ground ++ plat1 ++ plat2 ++ plat3 ++ plat4 ++ plat5 ++ plat6 ++ plat7
         ++ qBlocks ++ pipes ++ finish ++ flag ++ castle

    enemies =
         map mkG [8,25,45,68,90,106,122,145,165,182,200]
      ++ map mkK [32,55,85,118,150,170,195]
      ++ map mkP [(16,1),(57,1),(110,2),(175,1)]

    coins = mkCoins $
         [(15,7),(16,7)]           -- above plat1 Q blocks
      ++ [(38,8),(39,8),(40,8)]    -- above plat2 Q blocks
      ++ [(56,7),(57,7)]           -- above plat3 Q blocks
      ++ [(79,9),(80,9),(81,9),(82,9)] -- above plat4 Q blocks
      ++ [(109,7),(110,7),(111,7),(112,7)] -- above plat5 Q blocks
      ++ [(134,8),(135,8),(136,8)] -- above plat6 Q blocks
      ++ [(174,7),(175,7),(176,7)] -- above plat7 Q blocks

--------------------------------------------------------------------------------
-- World 2-4
-- Bowser's castle: longer and harder than 1-4.
-- Two lava corridors, three firebars, more enemies, longer bridge to Bowser.
--------------------------------------------------------------------------------

level2_4 :: Level
level2_4 = mkLevel tiles enemies coins [] firebars (ts*3) (ts*3) (95*ts) 2 4
  where
    -- Floor sections with two lava gaps
    floorA = mkGround 0  13
    floorB = mkGround 18 27
    floorC = mkGround 32 44
    floorD = mkGround 49 58
    floorE = mkGround 63 80
    lava1  = [Tile c (-2) Ground | c <- [14..17]]
    lava2  = [Tile c (-2) Ground | c <- [28..31]]
    lava3  = [Tile c (-2) Ground | c <- [45..48]]
    lava4  = [Tile c (-2) Ground | c <- [59..62]]

    -- Bridge over the final lava stretch, leading to Bowser
    bridge        = mkBridge 20 90
    bridgeSupport = mkBridgePosts [20,25,30,35,40,45,50,55,60,65,70,75,80,85,90]

    stairClimb = mkStairsUp 88 6
    axe        = [Tile 94 1 Axe]
    castle     = mkCastle 95

    tiles = floorA ++ floorB ++ floorC ++ floorD ++ floorE
         ++ lava1 ++ lava2 ++ lava3 ++ lava4
         ++ bridge ++ bridgeSupport
         ++ stairClimb ++ axe ++ castle

    -- Three firebars: staggered angles so they don't all line up
    firebars =
      [ Firebar (24*ts) (3*ts) 0.00 2.6 4
      , Firebar (44*ts) (3*ts) 1.05 2.2 5
      , Firebar (68*ts) (3*ts) 2.10 2.0 5
      ]

    enemies =
      [ Enemy (6*ts)  ts      (-80) 0 EAlive Goomba
      , Enemy (9*ts)  ts      (-80) 0 EAlive Goomba
      , Enemy (20*ts) (ts*2)  (-70) 0 EAlive Koopa  -- on bridge
      , Enemy (35*ts) (ts*2)  (-70) 0 EAlive Koopa  -- on bridge
      , Enemy (55*ts) (ts*2)  (-80) 0 EAlive Goomba -- on bridge
      , Enemy (58*ts) (ts*2)  (-80) 0 EAlive Goomba -- on bridge
      , mkBowser 80                                  -- Bowser on bridge
      ]

    coins = mkCoins
      [(5,2),(6,2),(7,2),(8,2),(22,2),(26,2),(34,2),(38,2),(50,2),(54,2)]

--------------------------------------------------------------------------------
-- All levels exported
--------------------------------------------------------------------------------

allLevels :: [Level]
allLevels = [ level1_1, level1_2, level1_3, level1_4
            , level2_1, level2_2, level2_3, level2_4
            ]