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
  :: [Tile] -> [Enemy] -> [(Float,Float,Bool)] -> [PUp] -> [Firebar] -> [MovingPlatform]
  -> Float -> Float -> Float -> Int -> Int -> Level
mkLevel ts_ es cs ps fs plats sx sy ex w n = Level ts_ es cs ps fs plats sx sy ex w n

initMarioFromLevel :: Level -> Mario
initMarioFromLevel lvl = Mario (lStartX lvl) (lStartY lvl) 0 0 False Small 1 0 0 0 False False ""

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
      -- Piranhas in each pipe: heights are pipe_height - 1 (row inside pipe top)
      ++ map mkP [(28,1),(38,2),(46,3),(57,3),(163,1)]

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
      , mkP (178, 2)
      , mkP (182, 2)
      , mkP (186, 2)
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
-- World 1-4
-- Bowser's castle: lava pits, two narrow bridged gaps, a brick-platform
-- obstacle with a firebar, Bowser's long bridge with a second firebar,
-- a staircase, axe, and castle.
--
-- Root cause of the old bug: mkRect Step 0 50 2 10 filled every column 0–50
-- at rows 2–10 with solid Step tiles, burying Mario (who spawns at row ~1.5)
-- inside a wall of blocks.  Those tiles and the side-wall lines are gone.
--
-- Geometry (all enemies verified clear of blocked cols):
--   floorA : cols  0–10  (starting run)
--   bridge1: cols 11–13  (row-1 Step over lava1)
--   floorB : cols 14–28  (mid section with brick platform obstacle)
--   bridge2: cols 29–31  (row-1 Step over lava2)
--   floorC : cols 32–40  (approach to Bowser's bridge)
--   bowserBridge: cols 41–70 (row-1 Step; Bowser patrols here)
--   stairs : cols 70–74  (mkStairsUp 70 5)
--   axe    : col  75, row 1
--   castle : cols 76–80
--------------------------------------------------------------------------------

level1_4 :: Level
level1_4 = mkLevel tiles enemies coins pups firebars [] (ts*3) (ts*1.5) (80*ts) 1 4
  where
    -- ── Ground sections ───────────────────────────────────────────────────
    floorA = mkGround 0  10   -- starting area
    floorB = mkGround 14 28   -- mid section (holds brick obstacle)
    floorC = mkGround 32 40   -- approach to Bowser's bridge

    -- ── Lava pits ─────────────────────────────────────────────────────────
    lava1 = [Tile c (-2) Ground | c <- [11..13]]
    lava2 = [Tile c (-2) Ground | c <- [29..31]]
    lava3 = [Tile c (-2) Ground | c <- [41..69]]

    -- ── Short bridges over the narrow pits ────────────────────────────────
    bridge1 = mkBridge 11 13
    bridge2 = mkBridge 29 31

    -- ── ? block on floorA (col 8, row 3) ─────────────────────────────────
    -- Gives a Mushroom to Small Mario or a Fire Flower to Big/Fire Mario.
    -- Reachable with a standing jump from the ground (jump height ~142px;
    -- row-3 bottom edge is at 3*ts = 96px — well within range).
    powerBlock = mkQPower 3 8 8

    -- ── Brick platform obstacle in floorB ─────────────────────────────────
    platform1 = mkPlatform 3 18 20

    -- ── Bowser's long bridge ──────────────────────────────────────────────
    bowserBridge = mkBridge 41 70
    bridgePosts  = mkBridgePosts [41,44,47,50,53,56,59,62,65,68]

    -- ── Staircase, axe, castle ────────────────────────────────────────────
    stairClimb = mkStairsUp 70 5
    axe        = [Tile 75 1 Axe]
    castle     = mkCastle 76

    tiles = floorA ++ floorB ++ floorC
         ++ lava1 ++ lava2 ++ lava3
         ++ bridge1 ++ bridge2
         ++ powerBlock ++ platform1
         ++ bowserBridge ++ bridgePosts
         ++ stairClimb ++ axe ++ castle

    -- ── Firebars ──────────────────────────────────────────────────────────
    firebars =
      [ Firebar (19*ts) (fromIntegral (4::Int)*ts) 0.00 2.4 4
      , Firebar (55*ts) (fromIntegral (2::Int)*ts) 1.30 2.0 5
      ]

    -- ── Power-ups ─────────────────────────────────────────────────────────
    -- Pre-placed Fire Flower sitting on floorC at col 36.
    -- Mario walks into it after crossing the second bridge to power up
    -- before facing Bowser.  pVY = 0 (already on the ground).
    pups = [ PUp (36*ts) ts 0 0 True FireFlower ]

    -- ── Enemies ───────────────────────────────────────────────────────────
    -- Goombas moved from cols 5 & 7 to cols 15 & 26 (floorB, past the
    -- first lava pit).  Mario now has ~3 seconds of free movement at the
    -- start before any enemy arrives.
    enemies =
      [ mkBowser 60 ]   -- only Bowser; this is a boss level

    -- ── Coins ─────────────────────────────────────────────────────────────
    coins = mkCoins
      [(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2)  -- floorA path
      ,(15,2),(16,2),(17,2)                        -- before platform1
      ,(33,2),(34,2),(35,2)                        -- floorC
      ,(43,2),(50,2),(57,2),(64,2)                 -- along Bowser's bridge
      ]

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
      ++ map mkP [(39,1),(54,2),(134,2),(150,3),(169,2)]

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
-- NOTE: The original 2-2 is an underwater level with Bloopers and Cheep-cheeps.
-- Swimming physics and those enemy types are not yet implemented, so this is a
-- placeholder underground level with a different layout from 1-2.
-- The ceiling is slightly lower, platforms form an S-curve, and enemies are
-- denser to reflect the increased difficulty of World 2.
--------------------------------------------------------------------------------

level2_2 :: Level
level2_2 = mkLevel tiles enemies coins [] [] [] (ts*3) (ts*1.5) (208*ts) 2 2
  where
    ground = mkGround 0 215

    entryPipe = mkPipe 14 2

    -- Tighter ceiling than 1-2 (row 10 instead of 11)
    caveCeiling = mkCeiling 10 18 192

    -- Early cave: ? block row + two ascending stair formations
    questRow = mkQPower 2 30 30 ++ mkPlatform 2 31 34
    stairA   = concat [ [Tile (42+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]
    stairB   = concat [ [Tile (50+i) r Brick | r <- [1..(i+1)]] | i <- [0..3] ]

    midShelfL  = mkPlatform 4 62 68 ++ mkPlatform 4 72 76
    midQBlocks = mkQPower 4 69 69 ++ mkQCoin 4 70 71
    highShelf  = mkPlatform 7 80 92
    highQ      = mkQPower 7 86 86 ++ mkQCoin 7 87 87

    lowerPlat  = mkPlatform 4 98 112
    lowerQ     = mkQPower 4 105 105 ++ mkQCoin 4 106 106
    upperPlat  = mkPlatform 7 115 130
    upperQ     = mkQPower 7 122 122 ++ mkQCoin 7 123 123
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
         map mkG [35,56,70,85,100,118,130,148,175,197]
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
level2_3 = mkLevel tiles enemies coins [] [] [] (ts*3) (ts*1.5) (213*ts) 2 3
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
         mkQPower 5 15 15 ++ mkQCoin 5 16 16
      ++ mkQPower 6 38 38 ++ mkQCoin 6 39 40
      ++ mkQPower 5 56 56 ++ mkQCoin 5 57 57
      ++ mkQPower 7 79 79 ++ mkQCoin 7 80 82
      ++ mkQPower 5 109 109 ++ mkQCoin 5 110 112
      ++ mkQPower 6 134 134 ++ mkQCoin 6 135 136
      ++ mkQPower 5 174 174 ++ mkQCoin 5 175 176

    -- Pipes at the edges of some segments (with Piranhas)
    pipes = mkPipeGroup [(16,2),(57,2),(110,3),(175,2)]

    finish = mkStairsUp 205 8
    flag   = mkFlag 213
    castle = mkCastle 216

    tiles = ground ++ plat1 ++ plat2 ++ plat3 ++ plat4 ++ plat5 ++ plat6 ++ plat7
         ++ qBlocks ++ pipes ++ finish ++ flag ++ castle

    enemies =
         map mkG [8,25,45,68,90,106,122,145,162,182,200]
      ++ map mkK [32,52,85,118,150,170,195]
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
level2_4 = mkLevel tiles enemies coins [] firebars [] (ts*3) (ts*3) (95*ts) 2 4
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