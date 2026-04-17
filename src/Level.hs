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
-- Enemies
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

--------------------------------------------------------------------------------
-- World 1-1
--------------------------------------------------------------------------------

level1_1 :: Level
level1_1 = mkLevel tiles enemies coins [] [] (ts*3) (ts*1.5) (204*ts) 1 1
  where
    ground = mkGround 0 211

    blocks =
         mkQLine 3 16 16
      ++ mkQLine 3 21 21
      ++ mkPlatform 3 22 22
      ++ mkQLine 3 23 23
      ++ mkPlatform 3 24 24
      ++ mkQLine 7 22 22

      ++ mkPlatform 3 78 78
      ++ mkQLine 3 79 79
      ++ mkPlatform 3 80 82
      ++ mkQLine 7 78 80

      ++ mkPlatform 3 91 92
      ++ mkQLine 3 93 93
      ++ mkPlatform 3 94 95

      ++ mkPlatform 3 100 100
      ++ mkQLine 3 101 101
      ++ mkPlatform 3 102 102

      ++ mkPlatform 3 107 109
      ++ mkPlatform 7 107 107
      ++ mkQLine 7 108 108
      ++ mkPlatform 7 109 110

    pipes = mkPipeGroup [(28,2),(38,3),(46,4),(57,4),(163,2)]
    stairs = mkStairsUp 127 4 ++ mkStairsDown 133 4 ++ mkStairsUp 140 4 ++ mkStairsDown 146 4
    finish = mkStairsUp 196 8
    flag = mkFlag 204
    castle = mkCastle 207

    tiles = ground ++ blocks ++ pipes ++ stairs ++ finish ++ flag ++ castle

    enemies =
         map mkG [20,22,37,40,57,59,80,82,100,102,110,116,150,152]
      ++ map mkK [60,92,130]
      ++ map mkP [(28,1),(38,2),(46,3),(57,3),(163,1)]

    coins = mkCoins
      ([(c,2) | c <- [1..4]] ++ [(21,5),(23,5),(79,5),(82,5),(93,5),(101,5),(108,9)])

--------------------------------------------------------------------------------
-- World 1-2
--------------------------------------------------------------------------------

level1_2 :: Level
level1_2 = mkLevel tiles enemies coins [] [] (ts*3) (ts*3) (208*ts) 1 2
  where
    ground = mkGround 0 208
    ceiling = mkCeiling 12 0 208

    bricks =
         mkPlatform 4 2 5
      ++ mkPlatform 4 7 10
      ++ mkPlatform 8 2 5
      ++ mkPlatform 8 7 10
      ++ mkQLine 4 16 16
      ++ mkPlatform 4 17 19
      ++ mkQLine 4 21 21
      ++ mkPlatform 4 22 24
      ++ mkPlatform 7 32 34
      ++ mkQLine 4 48 49
      ++ mkPlatform 8 48 49
      ++ mkPlatform 7 64 66
      ++ mkQLine 10 72 72
      ++ mkQLine 10 74 74
      ++ mkQLine 10 76 76
      ++ mkPlatform 4 96 97
      ++ mkPlatform 8 96 97
      ++ mkPlatform 4 112 113
      ++ mkPlatform 8 112 113
      ++ mkPlatform 4 128 130
      ++ mkPlatform 8 128 130
      ++ mkPlatform 4 144 145
      ++ mkPlatform 8 144 145
      ++ mkPlatform 4 160 162
      ++ mkPlatform 8 160 162
      ++ mkPlatform 4 176 177
      ++ mkPlatform 8 176 177
      ++ mkPlatform 4 192 193
      ++ mkPlatform 8 192 193

    pipes = mkPipeGroup [(28,2),(38,3),(46,4),(57,4),(100,2),(108,3)]
    warp  = mkPipeGroup [(68,2),(72,3),(76,4)]
    exitPipe = mkPipe 203 2 ++ [Tile 203 3 Step, Tile 204 3 Step]

    tiles = ground ++ ceiling ++ bricks ++ pipes ++ warp ++ exitPipe

    enemies =
         map mkG [16,18,36,38,52,54,80,82,100,102,120,122,144,146,160,162,180,182]
      ++ map mkK [64,96,128]
      ++ map mkP [(28,1),(38,2),(46,3),(57,3),(100,1),(108,2)]

    coins = mkCoins
      ([(c,2) | c <- [1..5]] ++ [(16,6),(17,6),(21,6),(22,6)] ++ [(48,6),(49,6)]
       ++ [(96,9),(97,9)] ++ [(144,9),(145,9)] ++ [(176,6),(177,6)])

--------------------------------------------------------------------------------
-- World 1-3
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

    bridge = mkBridge 16 39
    bridgeSupport = mkBridgePosts [16,19,22,25,28,31,34,37]

    secondRun = mkBridge 40 73
    secondSupport = mkBridgePosts [40,45,50,55,60,65,70,73]

    stairClimb = mkStairsUp 73 6
    axe = [Tile 77 1 Axe]
    castle = mkCastle 78

    tiles = floorA ++ floorB ++ floorC ++ lava ++ walls
         ++ bridge ++ bridgeSupport
         ++ secondRun ++ secondSupport
         ++ stairClimb ++ axe ++ castle

    firebars =
      [ Firebar (34*ts) (3*ts) 0.00 2.4 4
      , Firebar (58*ts) (3*ts) 1.30 2.0 5
      ]

    enemies =
      [ Enemy (25*ts) (ts*2) (-70) 0 EAlive Koopa
      , Enemy (20*ts) (ts*2) (-80) 0 EAlive Goomba
      , Enemy (30*ts) (ts*2) (-80) 0 EAlive Goomba
      ]

    coins = mkCoins [(5,2),(6,2),(7,2),(8,2),(9,2),(10,2),(20,2),(25,2),(30,2),(35,2)]

--------------------------------------------------------------------------------
-- All levels
--------------------------------------------------------------------------------

allLevels :: [Level]
allLevels = [level1_1, level1_2, level1_3, level1_4]