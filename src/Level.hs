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
mkQLine r c1 c2 = mkRow (QBlock QPowerUp) r c1 c2

mkQCoin :: Int -> Int -> Int -> [Tile]
mkQCoin r c1 c2 = mkRow (QBlock QCoin) r c1 c2

mkQPower :: Int -> Int -> Int -> [Tile]
mkQPower r c1 c2 = mkRow (QBlock QPowerUp) r c1 c2

mkUsedLine :: Int -> Int -> Int -> [Tile]
mkUsedLine r c1 c2 = mkRow Used r c1 c2

mkPipe :: Int -> Int -> [Tile]
mkPipe c h =
  [Tile c r t | (r,t) <- zip [1..h] (replicate (h-1) Pipe ++ [PipeTop])]
  ++ [Tile (c+1) r PipeR | r <- [1..h]]

-- | Piranha in a pipe.
--   Use the SAME values as mkPipe:
--   mkPipe c h  ==>  mkP (c, h)
--
--   eX is shifted by ts/2 so the plant is centered between the
--   left and right pipe columns.
--   eVY stores the fixed hidden/base Y position.
mkP :: (Int, Int) -> Enemy
mkP (c, h) =
  let baseY = fromIntegral h * ts - ts * 0.45
      x     = fromIntegral c * ts + ts / 2
  in Enemy x baseY 0 baseY (EPiranha 1.2 False) Piranha

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

mkCheep :: Int -> Int -> Int -> Enemy
mkCheep c r dir =
  Enemy
    (fromIntegral c * ts)
    (fromIntegral r * ts)
    (90 * fromIntegral dir)
    20
    EAlive
    CheepCheep

mkGreenCheep :: Int -> Int -> Int -> Enemy
mkGreenCheep c r dir =
  Enemy
    (fromIntegral c * ts)
    (fromIntegral r * ts)
    (90 * fromIntegral dir)
    20
    EAlive
    GreenCheep

mkJumpCheep :: Int -> Int -> Int -> Enemy
mkJumpCheep c r dir =
  Enemy
    (fromIntegral c * ts)
    (fromIntegral r * ts)
    (80 * fromIntegral dir)
    580
    EAlive
    JumpingCheep

mkBlooper :: Int -> Int -> Enemy
mkBlooper c r =
  Enemy
    (fromIntegral c * ts)
    (fromIntegral r * ts)
    (-35)
    0
    EAlive
    Blooper
    
mkLevel
  :: [Tile] -> [Enemy] -> [(Float,Float,Bool)] -> [PUp] -> [Firebar] -> [MovingPlatform]
  -> Float -> Float -> Float -> Int -> Int -> Level
mkLevel ts_ es cs ps fs plats sx sy ex w n = Level ts_ es cs ps fs plats sx sy ex w n

initMarioFromLevel :: Level -> Mario
initMarioFromLevel lvl = Mario (lStartX lvl) (lStartY lvl) 0 0 False Small 1 0 0 0 False False "" 0 False False False 0 Small

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

-- | Bowser: 2-tile-wide, spawns at column c.
--   y = ts*2 places him on top of the row-1 bridge tiles.
--   EBowser timers: fireTimer, jumpTimer, idleTimer, hitPoints.
mkBowser :: Int -> Enemy
mkBowser c =
  Enemy
    (fromIntegral c * ts)
    (ts * 2)
    (-55)
    0
    (EBowser 2.5 3.4 1.0 5)
    Bowser

--------------------------------------------------------------------------------
-- World 1-1
-- Reference: https://www.mariowiki.com/World_1-1
--------------------------------------------------------------------------------

level1_1 :: Level
level1_1 = mkLevel tiles enemies coins [] [] [] (ts*3) (ts*1.5) (198*ts) 1 1
  where
    blocks =
      -- Lone Q-block at col 16, row 4: power-up (Mushroom/Flower)
         mkQPower   4 16 16

      -- First brick cluster (cols 20–24, row 4): B ? B ? B
      -- Q at col 21: coin, Q at col 23: coin
      ++ mkPlatform 4 20 20
      ++ mkQCoin    4 21 21
      ++ mkPlatform 4 22 22
      ++ mkQCoin    4 23 23
      ++ mkPlatform 4 24 24
      -- Hidden single Q-block above col 22, row 8: power-up
      ++ mkQPower   8 22 22

      -- Second cluster (cols 77–79, row 4): B ? B
      -- Q at col 78: coin
      ++ mkPlatform 4 77 77
      ++ mkQCoin    4 78 78
      ++ mkPlatform 4 79 79
      ++ mkPlatform 4 94 94
      -- Row-8 shelf: 8 bricks (80–87), gap, 3 bricks (91–93), Q-block (94): power-up
      ++ mkPlatform 8 80 87
      ++ mkPlatform 8 91 93
      ++ mkQPower   8 94 94

      -- Third cluster (cols 100–101, row 4): B B
      ++ mkPlatform 4 100 101

      -- Fifth cluster (cols 106, 109, 112, row 4): three Q-blocks — all coins
      -- Plus bricks at col 118 row 4 and cols 129–130 row 4
      ++ mkQCoin    4 106 106
      ++ mkQCoin    4 109 109
      ++ mkQCoin    4 112 112
      ++ mkPlatform 4 118 118
      ++ mkPlatform 4 129 130
      -- Row-8 shelf: Q@109 power-up, BBB@121–123, B@128, QQ@129–130 coins, B@131
      ++ mkQPower   8 109 109
      ++ mkPlatform 8 121 123
      ++ mkPlatform 8 128 128
      ++ mkQCoin    8 129 130
      ++ mkPlatform 8 131 131

      -- End cluster (cols 168–171, row 4): B B ? B — Q is a coin
      ++ mkPlatform 4 168 169
      ++ mkQCoin    4 170 170
      ++ mkPlatform 4 171 171

    -- Ground with pits: three holes confirmed by pixel scan
    --   Hole 1: cols 69–70   (2 tiles)
    --   Hole 2: cols 86–88   (3 tiles)
    --   Hole 3: cols 153–154 (2 tiles)
    ground = mkGround  0  68
          ++ mkGround 71  85
          ++ mkGround 89 152
          ++ mkGround 155 211

    pipes  = mkPipeGroup [(28,2),(38,3),(46,4),(57,4),(163,2),(179,2)]

    -- Staircase pair 1: up 4 (cols 134–137) + down 4 (cols 140–143)
    -- Staircase pair 2: up 4 (cols 148–151) + cap col (152, h=4) + down 4 (cols 155–158)
    stairs = mkStairsUp 134 4 ++ mkStairsDown 140 4
          ++ mkStairsUp 148 4 ++ [Tile 152 r Step | r <- [1..4]]
          ++ mkStairsDown 155 4

    -- Final staircase: 8 steps up (cols 181–188) + cap column (189, h=8)
    finish = mkStairsUp 181 8 ++ [Tile 189 r Step | r <- [1..8]]
    flag   = mkFlag 198
    castle = mkCastle 202

    tiles = ground ++ blocks ++ pipes ++ stairs ++ finish ++ flag ++ castle

    enemies =
      -- Goombas placed on clear ground, away from pipes (28,38,46,57,163,179)
      -- Each pair is spaced at least 4 cols apart so they don't immediately collide.
         map mkG [22, 35, 42, 53, 62, 78, 84, 100, 106, 110, 116, 144, 160]
      -- Koopas in wider open stretches
      ++ map mkK [64, 92, 130]
      -- Only pipes 3 and 4 have piranhas, matching the original World 1-1
      ++ map mkP [(46,4),(57,4)]

    -- Coins: pre-placed coins are empty; ? block coins fly out on bump.
    coins = mkCoins []


--------------------------------------------------------------------------------
-- World 1-2

-- World 1-2 — pixel-accurate from SuperMarioBrosMap1-2.png (no-enemies version).
-- Coordinate system: col = image_x/16, row = 11-(image_y-272)/16, tile=16px.
-- All tile positions confirmed by sub-pixel color analysis of sprites.
--------------------------------------------------------------------------------

level1_2 :: Level
level1_2 = mkLevel tiles enemies coins [] [] platforms (ts*3) (ts*1.5) (200*ts) 1 2
  where
    -- ── Ground with pits ──────────────────────────────────────────────────
    -- Pits confirmed pixel-accurate: rows 0 and -1 both empty.
    -- The island at 140-142 and the ground at 145-152 are REMOVED —
    -- that entire section (138-159) is a pit crossed by moving platforms.
    ground = mkGround 0   79   -- start to pit 1
          ++ mkGround 83  119  -- after pit 1
          ++ mkGround 122 123  -- small island (staircase on top)
          ++ mkGround 126 137  -- main ground with ascending staircase
          ++ mkGround 160 191  -- warp room + exit pipes

    -- ── Cave ceiling: row 11, cols 6–137 (Brick — breakable) ────────────
    caveCeiling = mkRow Brick 11 6 137

    -- ── Five Q-blocks in a row at row 4 ──────────────────────────────────
    blockRow = mkQCoin 4 10 14

    -- ── Entrance pyramid (Step — unbreakable) ─────────────────────────────
    entrancePyramid =
         [Tile 17 1 Step]
      ++ [Tile 19 r Step | r <- [1,2]]
      ++ [Tile 21 r Step | r <- [1..3]]
      ++ [Tile 23 r Step | r <- [1..4]]
      ++ [Tile 25 r Step | r <- [1..4]]
      ++ [Tile 27 r Step | r <- [1..3]]
      ++ [Tile 29 5 Step]
      ++ [Tile 31 r Step | r <- [1..3]]
      ++ [Tile 33 r Step | r <- [1,2]]

    -- ── Platform cluster (cols 39–55) — Brick ────────────────────────────
    platCluster =
         [Tile 39 r Brick | r <- [4..6]]
      ++ [Tile 40 r Brick | r <- [4,5]]
      ++ [Tile 41 r Brick | r <- [4..6]] ++ [Tile 41 8 Brick]
      ++ [Tile 42 r Brick | r <- [6,8]]
      ++ [Tile 43 r Brick | r <- [6,8]]
      ++ [Tile 44 r Brick | r <- [4..6]] ++ [Tile 44 8 Brick]
      ++ [Tile 45 r Brick | r <- [4,5]]
      ++ [Tile 46 r Brick | r <- [4..6]]
      ++ [Tile 52 r Brick | r <- [4..8]]
      ++ [Tile 53 r Brick | r <- [4..8]]
      ++ [Tile 54 r Brick | r <- [2..4]] ++ [Tile 54 9 Brick]
      ++ [Tile 55 r Brick | r <- [2..4]] ++ [Tile 55 9 Brick]

    -- ── Bracket / arch structures (cols 58–89) — Brick ───────────────────
    brackets =
         [Tile c r Brick | c <- [58..61], r <- [4,9,10]]
      ++ [Tile c 5 Brick | c <- [58..61]]
      ++ [Tile c r Brick | c <- [62,63], r <- [4..10]]
      ++ [Tile 66 r Brick | r <- [9,10]]
      ++ [Tile 67 r Brick | r <- [4..10]]
      ++ [Tile 68 r Brick | r <- [4,5,9,10]]
      ++ [Tile 69 r Brick | r <- [4,5,9,10]]
      ++ [Tile c r Brick  | c <- [72,73], r <- [4..8]]
      ++ [Tile c 4 Brick  | c <- [76..79]]
      ++ [Tile c r Brick  | c <- [76..79], r <- [9,10]]
      ++ [Tile c r Brick  | c <- [84..89], r <- [5,6,8]]

    -- ── Warp-zone pipes (worlds 4/3/2) ───────────────────────────────────
    warpPipe4 = mkPipe 103 3
    warpPipe3 = mkPipe 109 4
    warpPipe2 = mkPipe 115 2

    -- ── Right staircase — Step (unbreakable) ──────────────────────────────
    rightStairs =
         [Tile c r Step | c <- [122,123], r <- [1..3]]
      ++ [Tile 133 1 Step]
      ++ [Tile 134 r Step | r <- [1,2]]
      ++ [Tile 135 r Step | r <- [1..3]]
      ++ [Tile 136 r Step | r <- [1..4]]
      ++ [Tile 137 r Step | r <- [1..4]]

    -- ── Warp-zone room (cols 160–176) — Brick + tall pipe ────────────────
    warpRoom =
         [Tile c r Brick | c <- [160..176], r <- [1..3]]
      ++ mkPipe 168 10
      ++ [Tile c r Brick | c <- [170..176], r <- [4..10]]

    -- ── Three underground exit pipes (h=3, all with Piranhas) ────────────
    exitPipeA = mkPipe 178 3
    exitPipeB = mkPipe 182 3
    exitPipeC = mkPipe 186 3

    -- ── Right cave wall — Brick ───────────────────────────────────────────
    rightWall = [Tile c r Brick | c <- [190,191], r <- [1..10]]

    -- ── Surface finish ────────────────────────────────────────────────────
    finish = mkStairsUp 192 8
    flag   = mkFlag 200
    castle = mkCastle 203

    tiles = ground
         ++ caveCeiling
         ++ blockRow
         ++ entrancePyramid
         ++ platCluster
         ++ brackets
         ++ warpPipe4 ++ warpPipe3 ++ warpPipe2
         ++ rightStairs
         ++ warpRoom
         ++ exitPipeA ++ exitPipeB ++ exitPipeC
         ++ rightWall
         ++ finish ++ flag ++ castle

    -- ── Moving platforms (lifts over the pit section cols 138–159) ────────
    -- Sprite is 120×26 px. mpWidth=4 gives ~128px collision width ≈ sprite width.
    -- Three platforms at different heights/phases, speed ~80 px/s vertical.
    platforms =
      [ MovingPlatform (138*ts) (2*ts)   80  (1*ts) (5*ts) 4
      , MovingPlatform (146*ts) (4*ts) (-80) (1*ts) (5*ts) 4
      , MovingPlatform (154*ts) (2*ts)   80  (1*ts) (5*ts) 4
      ]

    -- ── Enemies ───────────────────────────────────────────────────────────
    enemies =
      [ mkG  20
      , mkG  36
      , mkG  57
      , mkK  70
      , mkG  96
      , mkG 128
      , mkG 152
      -- No piranhas in the exit pipes — player must pass through them
      ]

    -- ── Coins ─────────────────────────────────────────────────────────────
    coins = mkCoins $
         [(64,7),(65,7),(66,7)]
      ++ [(74,7),(75,7)]
      ++ [(85,9),(86,9),(87,9),(88,9)]
      ++ [(118,5),(119,5)]

-- World 1-3
-- Treetop level: Mario walks across stacked brick platforms (tree canopy).
--------------------------------------------------------------------------------

level1_3 :: Level
level1_3 = mkLevel tiles enemies coins [] [] [] (ts*3) (ts*5) (244*ts) 1 3
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
         mkQPower 4 16 16 ++ mkQCoin  4 17 17
      ++ mkQPower 6 48 48
      ++ mkQPower 5 80 80 ++ mkQCoin  5 81 81
      ++ mkQPower 6 112 112
      ++ mkQPower 5 144 144 ++ mkQCoin 5 145 145
      ++ mkQPower 6 176 176
      ++ mkQPower 4 208 208 ++ mkQCoin 4 209 209

    tiles = ground ++ islands ++ jumps

    enemies = map mkG [14,20,50,82,114,146,178,210] ++ map mkK [64,128,192]
    coins = mkCoins
      ([(c,4) | c <- [26..29]] ++ [(c,5) | c <- [90..93]] ++ [(c,4) | c <- [122..125]]
       ++ [(c,5) | c <- [154..157]] ++ [(c,4) | c <- [186..189]] ++ [(c,4) | c <- [218..221]]
       ++ [(16,6),(17,6),(48,8),(80,7),(81,7),(112,8),(144,7),(145,7),(176,8),(208,6),(209,6)])

--------------------------------------------------------------------------------
-- World 1-4  (Bowser's Castle)
-- Pixel-accurate reconstruction from SuperMarioBrosMap1-4.png.
--
-- Coordinate mapping: NES tile col == game tile col (1:1).
--   NES 16px tiles scaled to game 32px tiles (tileScale = 32/48 handles sprite sizing).
--   NES rows inverted: game_row = 14 - NES_row.
--
-- Walkable floor surface = top of game row 5 = Y (5*ts + ts/2) in Gloss world coords.
-- Mario start Y = ts*6 so his feet land exactly on the floor surface.
--
-- Lava pits (row -2 Ground tiles, tiled by drawLava automatically):
--   Pit A: cols 13–14   Pit B: cols 26–28   Pit C: cols 32–34   Pit D: cols 128–140
--
-- Bridge: castle_bridge.png (624×64) auto-rendered by drawCastleBridge over lavaD.
--
-- Firebars: 7 total, pivots attached to ceiling or rising from floor.
--   Ceiling-hung (pivot at game row 9, chain hangs to row 7):
--     col 30 (floor post — rises from below), col 49, col 60, col 67
--   Floor posts (pivot at game row 6, chain up to row 4):
--     col 76, col 84
--   Pre-bridge (pivot at game row 9, near boss chamber): col 123
--
-- Bowser: 96×96 sprite (3×3 tiles), spawned at col 133 on the bridge.
-- Axe:    36×40 sprite, placed on the right wall at col 141, game row 5.
--------------------------------------------------------------------------------

level1_4 :: Level
level1_4 = mkLevel tiles enemies coins [] firebars [] (ts*3) (ts*6) (159*ts) 1 4
  where
    -- ── FLOOR ────────────────────────────────────────────────────────────
    -- Five solid sections; lava pits are the gaps between them.
    -- Rows -3 to 5. Lava pit columns excluded so no bricks appear inside pits.
    floorA = mkRect Ground   0  12 (-3) 5
    floorB = mkRect Ground  15  25 (-3) 5
    floorC = mkRect Ground  29  31 (-3) 5
    floorD = mkRect Ground  35 127 (-3) 5
          ++ mkRect Ground  30  30 (-3)  3
    floorE = mkRect Ground 141 159 (-3) 5

    -- ── BRIDGE (solid, walkable) ──────────────────────────────────────────
    -- Step tiles at game row 5 give Bowser and Mario a surface to stand on
    -- over lavaD. castle_bridge.png (624x64) is drawn on top visually.
    bridge = mkRow Step 5 128 140

    -- ── LAVA KILL TILES (row -2) ─────────────────────────────────────────
    -- drawLava tiles lava.png (144x72) across each contiguous run at natural size.
    -- drawCastleBridge additionally renders castle_bridge.png over lavaD.
    lavaA = [Tile c (-2) Ground | c <- [13..14]]
    lavaB = [Tile c (-2) Ground | c <- [26..28]]
    lavaC = [Tile c (-2) Ground | c <- [32..34]]
    lavaD = [Tile c (-2) Ground | c <- [128..140]]

    -- ── LEFT ENTRANCE WEDGE (NES rows 7–8 → game rows 6–7) ───────────────
    -- The castle entrance narrows toward the top: staircase step on the left wall.
    -- NES r8 (game r6): cols 0–3   NES r7 (game r7): cols 0–2
    wedge = mkRow Ground 7 0 2
         ++ mkRow Ground 6 0 3

    -- ── CEILING ───────────────────────────────────────────────────────────
    -- Derived from NES rows 2–5 (game rows 12–9), pixel-accurate runs.
    --
    -- game r12  (NES r2): full span, cols 0–159
    -- game r11  (NES r3): [0–23] [37–71] [97–103] [123–127] [142–143]
    -- game r10  (NES r4): [0–23] [37–71] [97–103] [123–127] [142–143]
    -- game r9   (NES r5): [23]   [37–71] [142–143]
    --   (cols 80,88 are firebar pivots embedded in the thick ceiling at r11)
    ceil12 = mkRow Ground 12  0 159
    ceil11 = mkRow Ground 11  0  23
          ++ mkRow Ground 11 37  71
          ++ mkRow Ground 11 97 103
          ++ mkRow Ground 11 123 127
          ++ mkRow Ground 11 142 143
    ceil10 = mkRow Ground 10  0  23
          ++ mkRow Ground 10 37  71
          ++ mkRow Ground 10 97 103
          ++ mkRow Ground 10 123 127
          ++ mkRow Ground 10 142 143
    ceil9  = [Tile 23 9 Ground]
          ++ mkRow Ground 9 37 71
          ++ mkRow Ground 9 142 143

    -- ── FIREBAR PIVOTS ────────────────────────────────────────────────────
    -- Each firebar has exactly ONE anchor block (the pivot tile the bar spins from).
    -- No chain tiles — the NES only shows the single pivot block, not a stack.
    -- FirebarTile renders as an empty/used block sprite (no collision).
    pivot30  = [Tile  30 9 FirebarTile]   -- floor post: pivot at ceiling level
    pivot49  = [Tile  49 9 FirebarTile]   -- ceiling-hung
    pivot60  = [Tile  60 9 FirebarTile]
    pivot67  = [Tile  67 9 FirebarTile]
    pivot76  = [Tile  76 6 FirebarTile]   -- floor post: pivot just above floor
    pivot84  = [Tile  84 6 FirebarTile]
    pivot123 = [Tile 123 9 FirebarTile]   -- pre-bridge, pivot at corridor ceiling

    -- ── AXE ──────────────────────────────────────────────────────────────
    -- Placed at col 141 row 6 (one tile above the floor, in the open corridor)
    -- so it is visible and not buried inside the floor wall tiles.
    -- castle_axe.png is 36x40px, rendered at natural size by drawTile.
    axe = [Tile 141 6 Axe]

    -- ── END CASTLE ────────────────────────────────────────────────────────
    castle = mkCastle 155

    tiles = floorA ++ floorB ++ floorC ++ floorD ++ floorE
         ++ bridge
         ++ lavaA ++ lavaB ++ lavaC ++ lavaD
         ++ wedge
         ++ ceil12 ++ ceil11 ++ ceil10 ++ ceil9
         ++ pivot30 ++ pivot49 ++ pivot60 ++ pivot67
         ++ pivot76 ++ pivot84 ++ pivot123
         ++ axe ++ castle

    -- ── FIREBARS ─────────────────────────────────────────────────────────
    -- Firebar x = col*ts + ts/2,  y = row*ts + ts/2.
    -- All firebars are length 4 (matching original NES 1-4).
    -- Phases staggered so they don't all point the same way at once.
    firebars =
      [ Firebar  (30*ts + ts/2) (9*ts + ts/2)   0.00 2.2 4  -- col 30  r9 floor post
      , Firebar  (49*ts + ts/2) (9*ts + ts/2)   0.00 2.0 4  -- col 49  r9 ceiling-hung
      , Firebar  (60*ts + ts/2) (9*ts + ts/2)   2.09 2.0 4  -- col 60  r9 offset phase
      , Firebar  (67*ts + ts/2) (9*ts + ts/2)   4.19 2.0 4  -- col 67  r9 offset phase
      , Firebar  (76*ts + ts/2) (6*ts + ts/2)   0.00 2.2 4  -- col 76  r6 floor post
      , Firebar  (84*ts + ts/2) (6*ts + ts/2)   3.14 2.2 4  -- col 84  r6 opposite phase
      , Firebar (123*ts + ts/2) (9*ts + ts/2)   1.05 2.0 4  -- col 123 r9 pre-bridge
      ]

    -- ── ENEMIES ───────────────────────────────────────────────────────────
    -- Bowser: 96x96 sprite, spawned at col 133 on the bridge.
    -- eY = ts*6 places his feet exactly on the bridge surface (game row 5 top).
    -- eVX = -55 so he paces left across the bridge toward Mario.
    enemies =
      [ Enemy (133 * ts) (ts * 6) (-55) 0 (EBowser 2.5 3.0 1.0 5) Bowser ]

    coins = mkCoins []

--------------------------------------------------------------------------------
-- World 2-1
-- Overworld redesign based on the original 2-1 map.
-- No underground bonus room and no fake climbable vine.
-- The sky route is a middle optional section, not a way to skip the whole level.
--------------------------------------------------------------------------------

level2_1 :: Level
level2_1 = mkLevel tiles enemies coins [] [] [] (ts*7) (ts*1.5) (205*ts) 2 1
  where
    -- Custom castle helper so the castle sits ON TOP of ground row 0,
    -- instead of overlapping/embedding into it like mkCastle does.
    castleOnGround c =
      mkRect Castle c (c+4) 1 5 ++
      [Tile x 6 Castle | x <- [c, c+2, c+4]]

    ground = mkGround 0 216

    startCastle = castleOnGround 0

    -- Small terrain detail after the starting castle.
    startSteps =
         [Tile 12 1 Step]
      ++ [Tile 13 r Step | r <- [1,2]]
      ++ [Tile 14 r Step | r <- [1..3]]
      ++ [Tile 15 r Step | r <- [1,2]]

    blocks =
      -- Early reachable power-up cluster.
         mkPlatform 3 21 21
      ++ mkQPower   3 22 22
      ++ mkPlatform 3 23 24

      -- Second block group.
      ++ mkPlatform 3 32 32
      ++ mkQCoin    3 33 33
      ++ mkPlatform 3 34 34
      ++ mkQPower   3 35 35
      ++ mkPlatform 3 36 36

      -- Mid-low cluster before pipes.
      ++ mkPlatform 4 47 47
      ++ mkQPower   4 48 48
      ++ mkPlatform 4 49 49
      ++ mkQCoin    4 50 50
      ++ mkPlatform 4 51 51

      -- More normal lower-level blocks so the bottom path still matters.
      ++ mkPlatform 3 64 65
      ++ mkQCoin    3 66 66
      ++ mkPlatform 3 67 68

      -- Middle access to the sky section.
      -- This replaces the fake vine. Mario reaches it by jumping platform to platform.
      ++ mkPlatform 2 82 85
      ++ mkPlatform 4 88 91
      ++ mkPlatform 6 94 98

      -- Sky platform only covers the middle section.
      -- It ends before the back half so Mario must drop down and continue the level.
      ++ mkRow Step 8 101 123

      -- Exit/drop-down helpers from the sky platform.
      ++ mkPlatform 6 124 127
      ++ mkPlatform 4 130 132

      -- Back-half block clusters after Mario drops back down.
      ++ mkPlatform 3 140 140
      ++ mkQPower   3 141 141
      ++ mkQCoin    3 142 142
      ++ mkPlatform 3 143 144

      ++ mkPlatform 4 156 156
      ++ mkQCoin    4 157 157
      ++ mkQPower   4 158 158
      ++ mkPlatform 4 159 160

      ++ mkPlatform 3 176 177
      ++ mkQCoin    3 178 178
      ++ mkPlatform 3 179 180

    -- Pipes adjusted so they do not block the sky access too much.
    pipes = mkPipeGroup
      [ (39,2)
      , (54,3)
      , (72,2)
      , (134,3)
      , (150,4)
      , (169,3)
      ]

    -- Tall final staircase, flag, and ending castle.
    finish = mkStairsUp 190 8 ++ [Tile 198 r Step | r <- [1..8]]
    flag   = mkFlag 205
    castle = castleOnGround 209

    tiles =
         ground
      ++ startCastle
      ++ startSteps
      ++ blocks
      ++ pipes
      ++ finish
      ++ flag
      ++ castle

    enemies =
         map mkG [18,27,44,60,77,87,105,118,137,147,162,181,187]
      ++ map mkK [34,69,112,154,176]
      ++ map mkP [(39,2),(54,3),(134,3),(169,3)]

    coins = mkCoins $
         -- Early coin lines above reachable blocks.
         [(21,5),(22,5),(23,5),(24,5)]
      ++ [(32,5),(33,5),(34,5),(35,5),(36,5)]
      ++ [(c,6) | c <- [47..51]]

      -- Lower path coins before the sky access.
      ++ [(64,5),(65,5),(66,5),(67,5),(68,5)]

      -- Coins guiding the player upward.
      ++ [(82,4),(83,4),(84,4),(85,4)]
      ++ [(88,6),(89,6),(90,6),(91,6)]
      ++ [(94,8),(95,8),(96,8),(97,8),(98,8)]

      -- Middle sky coins.
      ++ [(c,10) | c <- [101..116]]
      ++ [(c,11) | c <- [119..121]]

      -- Coins after dropping back down.
      ++ [(140,5),(141,5),(142,5),(143,5),(144,5)]
      ++ [(156,6),(157,6),(158,6),(159,6),(160,6)]
      ++ [(176,5),(177,5),(178,5),(179,5),(180,5)]

      -- Coins before the ending staircase.
      ++ [(c,5) | c <- [184..188]]

--------------------------------------------------------------------------------
-- World 2-2
-- Full underwater version.
-- No above-ground intro/exit for now.
-- Design goals:
--   • keep the whole level underwater
--   • keep the main swim path open and accessible
--   • remove Koopas from this level
--   • use Cheep-cheeps and Bloopers for underwater enemies
--   • keep pipes low so they do not block Mario's path
--   • place coins and power-ups in open/reachable water
--------------------------------------------------------------------------------

level2_2 :: Level
level2_2 = mkLevel tiles enemies coins [] [] [] (ts*3) (ts*4) (183*ts) 2 2
  where
    -- ── Ocean floor ───────────────────────────────────────────────────────
    -- Solid ground. No solid ceiling — the wave strip is purely visual;
    -- physicsMarioWater enforces the ceiling boundary in code.
    floorTiles = mkGround 0 191

    -- ── Floor-level raised platforms (pixel-verified mounds) ──────────────
    floorMound1 = [Tile c r Step | c <- [17..19], r <- [1..4]]
    floorMound2 = [Tile c r Step | c <- [41..42], r <- [1..4]]
    floorMound3 = [Tile c r Step | c <- [101..102], r <- [1..4]]

    -- ── Tall mid-level pillars ─────────────────────────────────────────────
    pillar1 = [Tile 155 r Step | r <- [1..7]]
    pillar2 = [Tile 163 r Step | r <- [1..7]]

    -- ── Ceiling overhangs (stalactites) ───────────────────────────────────
    ceilHang1 = [Tile c r Step | c <- [77..78], r <- [9..11]]
    ceilHang2 =  [Tile c 9 Step | c <- [130..138]]
              ++ [Tile c 10 Step | c <- [130..131]]
              ++ [Tile 130 11 Step]

    -- ── Right end wall (staircase + solid border) ─────────────────────────
    rightWall =
         [Tile c 1 Step | c <- [184..190]]
      ++ [Tile c 2 Step | c <- [185..190]]
      ++ [Tile c 3 Step | c <- [186..190]]
      ++ [Tile c r Step | c <- [187..190], r <- [4,8,9,10,11]]
      ++ [Tile c r Step | c <- [188..190], r <- [5,6,7]]
      ++ [Tile c 4 Step | c <- [171..175]]
      ++ [Tile c 4 Step | c <- [179..182]]
      ++ [Tile c 8 Step | c <- [171..175]]
      ++ [Tile c 8 Step | c <- [179..182]]

    -- ── Coral decorations (Coral = solid, rendered with coral sprite) ──────
    -- Columns chosen to be clear of all other solid geometry so coral can
    -- block Mario without creating impassable walls.
    -- Each entry: (col, height). Coral fills rows 1..height in that column.
    coral =
      concat
        [ [Tile c r Coral | r <- [1..h]]
        | (c, h) <- [ (10, 3), (11, 3)   -- short (1 sprite)
                    , (32, 6), (33, 6)   -- tall  (2 sprites)
                    , (88, 3), (89, 3)   -- short (1 sprite)
                    , (119, 6), (120, 6) -- tall  (2 sprites)
                    , (146, 3), (147, 3) -- short (1 sprite)
                    ]
        ]

    -- ── End marker ────────────────────────────────────────────────────────
    endMarker = [Tile 183 r FlagPole | r <- [1..10]]

    tiles =
         floorTiles
      ++ floorMound1 ++ floorMound2 ++ floorMound3
      ++ pillar1 ++ pillar2
      ++ ceilHang1 ++ ceilHang2
      ++ rightWall
      ++ coral
      ++ endMarker

    -- ── Enemies ───────────────────────────────────────────────────────────
    -- No pipes, no Piranhas. Mix of red and green Cheep-Cheeps + Bloopers.
    enemies =
         [ mkCheep     97  9 (-1)
         , mkCheep    127  2 (-1)
         , mkCheep    149  7   1
         , mkCheep    166  1   1
         , mkCheep    182  8 (-1)
         , mkCheep    185  5   1
         ]
      ++ [ mkGreenCheep  34  5 (-1)
         , mkGreenCheep  62  6 (-1)
         , mkGreenCheep  90  4   1
         , mkGreenCheep 124  6 (-1)
         , mkGreenCheep 150  5   1
         ]
      ++ [ mkBlooper  21  1
         , mkBlooper  45  2
         , mkBlooper  54  1
         , mkBlooper  75  2
         , mkBlooper  93  7
         , mkBlooper 100  2
         , mkBlooper 116  8
         , mkBlooper 144  2
         , mkBlooper 163  9
         , mkBlooper 174  6
         ]

    -- ── Coins ─────────────────────────────────────────────────────────────
    -- Solid tile columns that coins must NOT appear in:
    --   floorMounds: cols 17-19 rows 1-4, cols 41-42 rows 1-4, cols 101-102 rows 1-4
    --   pillars:     col 155 rows 1-7, col 163 rows 1-7
    --   ceilHang1:   cols 77-78 rows 9-11
    --   ceilHang2:   cols 130-138 row 9, cols 130-131 row 10, col 130 row 11
    --   rightWall:   cols 171-190 various
    --   coral:       cols 10-11,32-33,88-89,119-120,146-147 rows 1..h
    -- All coin positions below have been manually checked against these.
    coins = mkCoins $
         [(13,1),(14,1)]
      ++ [(26,7),(27,7),(28,7)]
      ++ [(35,1),(36,1),(37,1)]
      ++ [(50,5),(51,5),(52,5)]
      ++ [(66,2),(67,2),(68,2)]
      ++ [(72,5),(73,5),(74,5),(75,5)]
      ++ [(80,3),(81,3),(82,3)]
      ++ [(96,9)]
      ++ [(100,1),(103,1),(104,1)]
      ++ [(107,6),(108,6),(109,6)]
      ++ [(112,6),(113,6),(114,6)]
      ++ [(121,2),(122,2),(123,2)]
      ++ [(127,2)]
      ++ [(132,1),(133,1),(134,1),(135,1)]
      ++ [(141,2),(142,2),(143,2)]
      ++ [(149,6)]
      ++ [(158,3),(159,3),(160,3)]
      ++ [(163,9)]
      ++ [(166,1)]
      ++ [(174,5)]
      ++ [(182,9)]

--------------------------------------------------------------------------------
-- World 2-3
-- Bridge / athletic redesign.
-- Final polish:
--   • longer map length restored
--   • varied but fair bridge gaps
--   • coin arcs guide Mario over jumps
--   • no coins directly on ? blocks
--   • middle platform is intentional and supported
--   • fewer Cheep-cheeps so it is less overwhelming
--   • flat recovery section before final staircase
--------------------------------------------------------------------------------

level2_3 :: Level
level2_3 = mkLevel tiles enemies coins [] [] [] (ts*7) (ts*1.5) (198*ts) 2 3
  where
    castleOnGround c =
      mkRect Castle c (c+4) 1 5 ++
      [Tile x 6 Castle | x <- [c, c+2, c+4]]

    bridge r c1 c2 = mkRow Step r c1 c2

    -- Support posts continue below the visible screen so they do not look cut off.
    posts r cols =
      concat [ [Tile c y Step | y <- [-4..r-1]] | c <- cols ]

    -- Only the start and ending have normal ground.
    startGround = mkGround 0 17

    -- Recovery ground before the final staircase.
    -- This gives Mario a safe stretch after the bridge section.
    endGround = mkGround 166 216

    startCastle = castleOnGround 0

    -- Start staircase onto the first bridge.
    startStairs =
         [Tile 10 1 Step]
      ++ [Tile 11 r Step | r <- [1,2]]
      ++ [Tile 12 r Step | r <- [1..3]]
      ++ [Tile 13 r Step | r <- [1..4]]

    -- Main bridge sections.
    -- Gaps vary from 3-4 tiles, so they feel more meaningful without being unfair.
    bridge1 = bridge 4 13 30
    bridge2 = bridge 4 34 53
    bridge3 = bridge 4 58 77
    bridge4 = bridge 4 83 103
    bridge5 = bridge 4 108 128
    bridge6 = bridge 4 133 156

    -- Intentional middle platform:
    -- placed under the middle bridge gap as a visual feature and safety route.
    middlePlatforms =
         bridge 2 80 91
      ++ bridge 2 158 164

    bridgeSupports =
         posts 4 [14, 22, 30]
      ++ posts 4 [35, 44, 53]
      ++ posts 4 [59, 68, 77]
      ++ posts 4 [84, 94, 103]
      ++ posts 4 [109, 119, 128]
      ++ posts 4 [134, 146, 156]
      ++ posts 2 [80, 91, 160, 164]

    -- Minimal ? blocks.
    -- Coin list below avoids these exact coordinates.
    blocks =
         mkQPower 7 24 24
      ++ mkQCoin  7 25 26

      ++ mkQPower 7 112 112
      ++ mkQCoin  7 113 114

    -- Final staircase is close to the flag, but the whole level stays long.
    finish = mkStairsUp 187 8 ++ [Tile 195 r Step | r <- [1..8]]
    flag   = mkFlag 198
    castle = castleOnGround 203

    tiles =
         startGround
      ++ endGround
      ++ startCastle
      ++ startStairs
      ++ bridge1
      ++ bridge2
      ++ bridge3
      ++ bridge4
      ++ bridge5
      ++ bridge6
      ++ middlePlatforms
      ++ bridgeSupports
      ++ blocks
      ++ finish
      ++ flag
      ++ castle

    -- Reduced enemy count so the level is challenging but not overwhelming.
    enemies =
      [ mkJumpCheep 22  (-2) (-1)
      , mkJumpCheep 42  (-2) 1
      , mkJumpCheep 66  (-2) (-1)
      , mkJumpCheep 90  (-2) 1
      , mkJumpCheep 116 (-2) (-1)
      , mkJumpCheep 138 (-2) 1
      , mkJumpCheep 150 (-2) (-1)
      , mkJumpCheep 162 (-2) 1
      ]

    coins = mkCoins $
         -- Early bridge coins, not on ? blocks at row 7 cols 24-26.
         [(18,7),(19,7),(20,7),(21,7)]
      ++ [(27,8),(28,8),(29,8)]

      -- Coin arcs over bridge gaps.
      ++ [(31,6),(32,7),(33,7),(34,6)]
      ++ [(54,6),(55,7),(56,7),(57,6)]
      ++ [(78,6),(79,7),(80,7),(81,7),(82,6)]
      ++ [(104,6),(105,7),(106,7),(107,6)]
      ++ [(129,6),(130,7),(131,7),(132,6)]

      -- Coins that make the middle platform feel intentional.
      ++ [(83,5),(84,5),(85,5),(86,5)]
      ++ [(88,5),(89,5),(90,5)]

      -- Middle bridge coins, avoiding ? blocks at row 7 cols 112-114.
      ++ [(62,7),(63,7),(64,7)]
      ++ [(110,8),(111,8),(115,8)]
      ++ [(121,8),(122,8),(123,8)]

      -- Back-half coins.
      ++ [(136,7),(137,7),(138,7)]
      ++ [(146,8),(147,8),(148,8)]
      ++ [(158,5),(159,5),(160,5),(161,5)]

      -- Recovery section before final staircase.
      ++ [(168,4),(169,4),(170,4),(171,4)]
      ++ [(178,4),(179,4),(180,4)]

--------------------------------------------------------------------------------
-- World 2-4
-- Castle redesign based on the original SMB 2-4 map.
-- More accurate pass:
--   • cleaner castle corridor layout
--   • firebars are centered on visible anchor blocks
--   • no full-height blocking walls in Mario's path
--   • more intentional lava/bridge sections
--   • more reachable power-up chances before Bowser
--   • distinct Bowser bridge + axe room
--------------------------------------------------------------------------------

level2_4 :: Level
level2_4 = mkLevel tiles enemies coins [] firebars [] (ts*3) (ts*1.5) (148*ts) 2 4
  where
    -- GameState treats row -2 Ground tiles as lava.
    lava c1 c2 = [Tile c (-2) Ground | c <- [c1..c2]]

    -- Center firebars on tile coordinates so they look attached to blocks.
    firebarAt c r ang spd len =
      Firebar
        (fromIntegral c * ts + ts/2)
        (fromIntegral r * ts + ts/2)
        ang spd len

    -- Support posts continue below the visible area.
    posts cols =
      concat [ [Tile c r Step | r <- [-4..0]] | c <- cols ]

    -- Main castle ceiling.
    -- These make the level feel enclosed without creating impossible walls.
    ceiling =
         mkRow Step 10 0 37
      ++ mkRow Step 10 43 83
      ++ mkRow Step 10 89 139

      -- Lower ceiling chunks like the original castle corridors.
      ++ mkRow Step 8  0 9
      ++ mkRow Step 8  18 26
      ++ mkRow Step 8  47 57
      ++ mkRow Step 8  70 82
      ++ mkRow Step 8  94 104
      ++ mkRow Step 8  118 132

      -- Left entrance wall only, behind Mario's path.
      ++ [Tile 0 r Step | r <- [1..9]]
      ++ [Tile 1 r Step | r <- [6..9]]

    -- Safe floor sections.
    floorA = mkGround 0 16
    floorB = mkGround 24 43
    floorC = mkGround 52 72
    floorD = mkGround 81 104

    -- Lava sections between safe floors.
    lavaA = lava 17 23
    lavaB = lava 44 51
    lavaC = lava 73 80
    lavaFinal = lava 105 140

    -- Small bridges/platforms over lava.
    -- These are simple and readable instead of cluttered.
    lavaPlatforms =
         mkRow Step 1 18 21
      ++ mkRow Step 2 46 49
      ++ mkRow Step 2 75 78

    -- Middle castle structures.
    -- Kept clear of the main walking lane.
    middlePlatforms =
         mkRow Step 4 55 60
      ++ mkRow Step 5 63 67
      ++ mkRow Step 4 92 101

    -- Visible anchor blocks for firebars.
    -- Each firebar below is centered on one of these blocks.
    firebarAnchors =
         [Tile 20 2 Step]     -- early lava firebar
      ++ [Tile 56 4 Step]     -- middle platform firebar
      ++ [Tile 96 4 Step]     -- pre-Bowser corridor firebar
      ++ [Tile 121 1 Step]    -- Bowser bridge firebar, part of bridge area

    -- Power-up blocks.
    -- QPower gives Mushroom if Small, Fire Flower if Big/Fire.
    blocks =
         -- Early power-up.
         mkQPower 3 8 8
      ++ mkQCoin  3 9 10

         -- Middle power-up.
      ++ mkQPower 4 32 32
      ++ mkQCoin  4 33 34

         -- Pre-Bowser power-up.
      ++ mkQPower 4 96 96
      ++ mkQCoin  4 97 98

    -- Small recovery / approach before the Bowser bridge.
    bridgeApproach =
         mkGround 100 104

    -- Final Bowser bridge over lava.
    bowserBridge = mkBridge 105 136
    bowserBridgePosts = posts [105,109,113,117,121,125,129,133,136]

    -- Axe and ending room.
    axe = [Tile 140 1 Axe]

    endRoom =
         mkGround 142 150
      ++ mkRow Step 10 142 150
      ++ [Tile 142 r Step | r <- [1..9]]
      ++ mkCastle 145

    tiles =
         ceiling
      ++ floorA ++ floorB ++ floorC ++ floorD
      ++ lavaA ++ lavaB ++ lavaC ++ lavaFinal
      ++ lavaPlatforms
      ++ middlePlatforms
      ++ firebarAnchors
      ++ blocks
      ++ bridgeApproach
      ++ bowserBridge
      ++ bowserBridgePosts
      ++ axe
      ++ endRoom

    -- Firebars centered on visible anchor blocks.
    firebars =
      [ firebarAt 20 2 0.00 2.2 3
      , firebarAt 56 4 1.10 2.0 3
      , firebarAt 96 4 2.40 2.1 3
      , firebarAt 121 1 1.40 2.0 4
      ]

    -- Castle danger comes from lava, firebars, and Bowser.
    enemies =
      [ mkBowser 124 ]

    coins = mkCoins
      [ -- early corridor coins
        (6,4),(7,4)

        -- early power-up guide coins
      , (8,5),(9,5),(10,5)

        -- first lava section reward
      , (18,4),(19,4),(20,4)

        -- middle power-up guide coins
      , (31,6),(32,6),(33,6),(34,6)

        -- middle platform coins
      , (55,6),(56,6),(57,6)
      , (64,7),(65,7),(66,7)

        -- third lava / pre-Bowser coins
      , (76,5),(77,5),(78,5)
      , (95,6),(96,6),(97,6),(98,6)

        -- Bowser bridge coins, spaced out
      , (110,3),(118,3),(126,3)
      ]
--------------------------------------------------------------------------------
-- All levels exported
--------------------------------------------------------------------------------

allLevels :: [Level]
allLevels = [ level1_1, level1_2, level1_3, level1_4
            , level2_1, level2_2, level2_3, level2_4
            ]