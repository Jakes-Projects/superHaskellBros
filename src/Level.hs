module Level (allLevels, initMarioFromLevel) where

import Constants (ts)
import Types

-- Helper to build a level
mkLevel :: [Tile] -> [Enemy] -> [(Float,Float,Bool)] -> [PUp] -> Float -> Float -> Float -> Int -> Int -> Level
mkLevel ts_ es cs ps sx sy ex w n = Level ts_ es cs ps sx sy ex w n

-- List of all levels
allLevels :: [Level]
allLevels = [level1_1, level1_2, level1_3, level1_4]

initMarioFromLevel :: Level -> Mario
initMarioFromLevel lvl = Mario (lStartX lvl) (lStartY lvl) 0 0 False Small 1 0 0

-- World 1-1 (unchanged from your current mkTiles, etc.)
level1_1 :: Level
level1_1 = mkLevel tiles1_1 enemies1_1 coins1_1 [] (ts*3) (ts*1.5) (204*ts) 1 1
  where
    tiles1_1 = ground ++ structs ++ pipes ++ stairs ++ flag ++ castle
      where
        ground = [Tile c r Ground | c <- [0..211], r <- [0,(-1)]]
        structs =
          [ Tile 16 3 QBlock, Tile 20 3 Brick, Tile 21 3 QBlock, Tile 22 3 Brick, Tile 23 3 QBlock, Tile 24 3 Brick
          , Tile 22 7 QBlock, Tile 78 3 Brick, Tile 79 3 QBlock, Tile 80 3 Brick, Tile 81 3 Brick, Tile 82 3 QBlock
          , Tile 83 3 Brick, Tile 78 7 QBlock, Tile 79 7 QBlock, Tile 80 7 QBlock, Tile 81 7 Brick
          , Tile 91 3 Brick, Tile 92 3 Brick, Tile 93 3 QBlock, Tile 94 3 Brick, Tile 95 3 Brick
          , Tile 100 3 Brick, Tile 101 3 QBlock, Tile 102 3 Brick, Tile 107 3 Brick, Tile 108 3 Brick, Tile 109 3 Brick
          , Tile 107 7 Brick, Tile 108 7 QBlock, Tile 109 7 Brick, Tile 110 7 Brick
          ]
        pipes = concatMap mkPipe [(28,2),(38,3),(46,4),(57,4),(163,2)]
        mkPipe (col, height) =
          [Tile col r t | (r,t) <- zip [1..height] (replicate (height-1) Pipe ++ [PipeTop])]
          ++ [Tile (col+1) r PipeR | r <- [1..height]]
        stairs = stairU 127 4 ++ stairD 133 4 ++ stairU 140 4 ++ stairD 146 4 ++
                 [Tile (196+i) r Ground | i <- [0..7], r <- [1..i+1]]
        stairU c h = [Tile (c+i) r (if i == r-1 then SlopeRight else Ground) | i <- [0..h-1], r <- [1..i+1]]
        stairD c h = [Tile (c+i) r (if i == (h - r) then SlopeLeft else Ground) | i <- [0..h-1], r <- [1..(h-i)]]
        flag = [Tile 204 r FlagPole | r <- [1..10]] ++ [Tile 204 0 FlagBase]
        castle = [Tile c r Castle | c <- [207..211], r <- [0..4]] ++ [Tile c 5 Castle | c <- [207,209,211]]
    enemies1_1 = map mkG [20,22,37,40,57,59,80,82,100,102,110,116,150,152] ++
                 map mkK [60,92,130] ++
                 map mkP [(28,1),(38,2),(46,3),(57,3),(163,1)]
      where
        mkG c = Enemy (c*ts) ts (-80) 0 EAlive Goomba
        mkK c = Enemy (c*ts) ts (-70) 0 EAlive Koopa
        mkP (c,r) = Enemy (c*ts) (fromIntegral r * ts) 0 0 (EPiranha 0.0 False) Piranha
    coins1_1 = [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False) | (c,r) <-
                [(c,2) | c <- [1..4]] ++ [(21,5),(23,5),(79,5),(82,5),(93,5),(101,5),(108,9)]]

-- World 1-2 (Underground with warp zone)
level1_2 :: Level
level1_2 = mkLevel tiles1_2 enemies1_2 coins1_2 [] (ts*3) (ts*3) (208*ts) 1 2
  where
    -- Ground layer (rows 0 and -1)
    ground = [Tile c r Ground | c <- [0..208], r <- [0,(-1)]]

    -- Ceiling of bricks (row 12) across the entire level
    ceilingTiles = [Tile c 12 Brick | c <- [0..208]]

    -- Brick formations and ? blocks matching original 1-2
    bricksAndBlocks =
      -- Starting area: bricks at rows 4 and 8, ? blocks
      [ Tile 2 4 Brick, Tile 3 4 Brick, Tile 4 4 Brick, Tile 5 4 Brick
      , Tile 7 4 Brick, Tile 8 4 Brick, Tile 9 4 Brick, Tile 10 4 Brick
      , Tile 2 8 Brick, Tile 3 8 Brick, Tile 4 8 Brick, Tile 5 8 Brick
      , Tile 7 8 Brick, Tile 8 8 Brick, Tile 9 8 Brick, Tile 10 8 Brick
      , Tile 16 4 QBlock, Tile 17 4 Brick, Tile 18 4 Brick, Tile 19 4 Brick
      , Tile 21 4 QBlock, Tile 22 4 Brick, Tile 23 4 Brick, Tile 24 4 Brick
      , Tile 16 8 Brick, Tile 17 8 Brick, Tile 18 8 Brick, Tile 19 8 Brick
      , Tile 21 8 Brick, Tile 22 8 Brick, Tile 23 8 Brick, Tile 24 8 Brick
      -- Mid section: bricks and ? blocks at various heights
      , Tile 32 7 Brick, Tile 33 7 Brick, Tile 34 7 Brick
      , Tile 48 4 QBlock, Tile 49 4 QBlock
      , Tile 48 8 Brick, Tile 49 8 Brick
      , Tile 64 7 Brick, Tile 65 7 Brick, Tile 66 7 Brick
      , Tile 80 4 Brick, Tile 81 4 Brick, Tile 82 4 Brick
      , Tile 80 8 Brick, Tile 81 8 Brick, Tile 82 8 Brick
      , Tile 96 7 QBlock, Tile 97 7 QBlock
      , Tile 96 11 Brick, Tile 97 11 Brick
      , Tile 112 4 Brick, Tile 113 4 Brick
      , Tile 112 8 Brick, Tile 113 8 Brick
      -- Area before warp zone
      , Tile 128 4 Brick, Tile 129 4 Brick, Tile 130 4 Brick
      , Tile 128 8 Brick, Tile 129 8 Brick, Tile 130 8 Brick
      , Tile 144 7 QBlock, Tile 145 7 QBlock
      , Tile 144 11 Brick, Tile 145 11 Brick
      -- Final stretch
      , Tile 160 4 Brick, Tile 161 4 Brick, Tile 162 4 Brick
      , Tile 160 8 Brick, Tile 161 8 Brick, Tile 162 8 Brick
      , Tile 176 4 QBlock, Tile 177 4 QBlock
      , Tile 176 8 Brick, Tile 177 8 Brick
      , Tile 192 4 Brick, Tile 193 4 Brick
      , Tile 192 8 Brick, Tile 193 8 Brick
      ]

    -- Standard pipes (no warp, just obstacles)
    pipes = concatMap mkPipe [(28,2), (38,3), (46,4), (57,4), (100,2), (108,3)]
      where
        mkPipe (col, height) =
          [Tile col r t | (r,t) <- zip [1..height] (replicate (height-1) Pipe ++ [PipeTop])]
          ++ [Tile (col+1) r PipeR | r <- [1..height]]

    -- Warp zone pipes (at columns 68, 72, 76) – for now they are just solid pipes
    warpPipes = concatMap mkPipeWarp [(68,2), (72,3), (76,4)]
      where
        mkPipeWarp (col, height) =
          [Tile col r t | (r,t) <- zip [1..height] (replicate (height-1) Pipe ++ [PipeTop])]
          ++ [Tile (col+1) r PipeR | r <- [1..height]]

    -- Exit pipe at the far right (columns 203-204)
    exitPipe =
      [ Tile 203 1 Pipe, Tile 203 2 PipeTop, Tile 204 1 PipeR, Tile 204 2 PipeR
      , Tile 203 3 Brick, Tile 204 3 Brick   -- decorative
      ]

    -- Combine all tiles
    tiles1_2 = ground ++ ceilingTiles ++ bricksAndBlocks ++ pipes ++ warpPipes ++ exitPipe

    -- Enemies: Goombas, Koopas, and Piranhas
    enemies1_2 = map mkG [16, 18, 36, 38, 52, 54, 80, 82, 100, 102, 120, 122, 144, 146, 160, 162, 180, 182]
                 ++ map mkK [64, 96, 128]
                 ++ map mkP [(28,1), (38,2), (46,3), (57,3), (100,1), (108,2)]
      where
        mkG c = Enemy (c*ts) ts (-80) 0 EAlive Goomba
        mkK c = Enemy (c*ts) ts (-70) 0 EAlive Koopa
        mkP (c,r) = Enemy (c*ts) (fromIntegral r * ts) 0 0 (EPiranha 0.0 False) Piranha

    -- Coins: scattered throughout, some inside ? blocks
    coins1_2 = [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False) | (c,r) <-
                [ (c,2) | c <- [1..5] ]
                ++ [ (16,6), (17,6), (21,6), (22,6) ]
                ++ [ (48,6), (49,6) ]
                ++ [ (96,9), (97,9) ]
                ++ [ (144,9), (145,9) ]
                ++ [ (176,6), (177,6) ]
              ]
 

 -- World 1-3 (Treetops / Athletic)
level1_3 :: Level
level1_3 = mkLevel tiles1_3 enemies1_3 coins1_3 [] (ts*3) (ts*5) (244*ts) 1 3
  where
    -- Gaps where there is no ground (rows 0 and -1)
    gapCols = [20..24] ++ [52..56] ++ [84..88] ++ [116..120] ++ [148..152] ++ [180..184] ++ [212..216]
    isGap c = c `elem` gapCols

    ground = [Tile c r Ground | c <- [0..244], r <- [0,(-1)], not (isGap c)]

    -- Floating platforms (rows 3-6) to jump across gaps
    platforms =
      [ Tile 26 3 Brick, Tile 27 3 Brick, Tile 28 3 Brick, Tile 29 3 Brick
      , Tile 26 4 Brick, Tile 27 4 Brick, Tile 28 4 Brick, Tile 29 4 Brick
      , Tile 58 3 Brick, Tile 59 3 Brick, Tile 60 3 Brick
      , Tile 58 4 Brick, Tile 59 4 Brick, Tile 60 4 Brick
      , Tile 90 5 Brick, Tile 91 5 Brick, Tile 92 5 Brick, Tile 93 5 Brick
      , Tile 90 6 Brick, Tile 91 6 Brick, Tile 92 6 Brick, Tile 93 6 Brick
      , Tile 122 3 Brick, Tile 123 3 Brick, Tile 124 3 Brick, Tile 125 3 Brick
      , Tile 122 4 Brick, Tile 123 4 Brick, Tile 124 4 Brick, Tile 125 4 Brick
      , Tile 154 5 Brick, Tile 155 5 Brick, Tile 156 5 Brick, Tile 157 5 Brick
      , Tile 154 6 Brick, Tile 155 6 Brick, Tile 156 6 Brick, Tile 157 6 Brick
      , Tile 186 3 Brick, Tile 187 3 Brick, Tile 188 3 Brick, Tile 189 3 Brick
      , Tile 186 4 Brick, Tile 187 4 Brick, Tile 188 4 Brick, Tile 189 4 Brick
      , Tile 218 3 Brick, Tile 219 3 Brick, Tile 220 3 Brick, Tile 221 3 Brick
      , Tile 218 4 Brick, Tile 219 4 Brick, Tile 220 4 Brick, Tile 221 4 Brick
      ]

    -- High tree-like structures
    trees =
      [ Tile 10 5 Brick, Tile 11 5 Brick, Tile 12 5 Brick
      , Tile 10 6 Brick, Tile 11 6 Brick, Tile 12 6 Brick
      , Tile 40 7 Brick, Tile 41 7 Brick, Tile 42 7 Brick
      , Tile 40 8 Brick, Tile 41 8 Brick, Tile 42 8 Brick
      , Tile 72 5 Brick, Tile 73 5 Brick, Tile 74 5 Brick
      , Tile 72 6 Brick, Tile 73 6 Brick, Tile 74 6 Brick
      , Tile 104 7 Brick, Tile 105 7 Brick, Tile 106 7 Brick
      , Tile 104 8 Brick, Tile 105 8 Brick, Tile 106 8 Brick
      , Tile 136 5 Brick, Tile 137 5 Brick, Tile 138 5 Brick
      , Tile 136 6 Brick, Tile 137 6 Brick, Tile 138 6 Brick
      , Tile 168 7 Brick, Tile 169 7 Brick, Tile 170 7 Brick
      , Tile 168 8 Brick, Tile 169 8 Brick, Tile 170 8 Brick
      , Tile 200 5 Brick, Tile 201 5 Brick, Tile 202 5 Brick
      , Tile 200 6 Brick, Tile 201 6 Brick, Tile 202 6 Brick
      ]

    -- Question blocks and coin bricks
    specials =
      [ Tile 16 4 QBlock, Tile 17 4 QBlock
      , Tile 48 6 QBlock
      , Tile 80 5 QBlock, Tile 81 5 QBlock
      , Tile 112 6 QBlock
      , Tile 144 5 QBlock, Tile 145 5 QBlock
      , Tile 176 6 QBlock
      , Tile 208 4 QBlock, Tile 209 4 QBlock
      ]

    -- Flagpole and castle at the end
    flag = [Tile 240 r FlagPole | r <- [1..10]] ++ [Tile 240 0 FlagBase]
    castle = [Tile c r Castle | c <- [243..247], r <- [0..4]] ++ [Tile c 5 Castle | c <- [243,245,247]]

    tiles1_3 = ground ++ platforms ++ trees ++ specials ++ flag ++ castle

    -- Enemies: Goombas, Koopas on platforms
    enemies1_3 = map mkG [16, 18, 50, 82, 114, 146, 178, 210]
                 ++ map mkK [64, 128, 192]
      where
        mkG c = Enemy (c*ts) (ts*2) (-80) 0 EAlive Goomba
        mkK c = Enemy (c*ts) (ts*2) (-70) 0 EAlive Koopa

    -- Coins: scattered across platforms
    coins1_3 = [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False) | (c,r) <-
                [ (c,4) | c <- [26..29] ]
                ++ [ (c,5) | c <- [90..93] ]
                ++ [ (c,4) | c <- [122..125] ]
                ++ [ (c,5) | c <- [154..157] ]
                ++ [ (c,4) | c <- [186..189] ]
                ++ [ (c,4) | c <- [218..221] ]
                ++ [ (16,6), (17,6), (48,8), (80,7), (81,7), (112,8), (144,7), (145,7), (176,8), (208,6), (209,6) ]
              ]

level1_4 :: Level
level1_4 = mkLevel tiles1_4 enemies1_4 coins1_4 [] (ts*3) (ts*3) (80*ts) 1 4
  where
    -- Ground at start and end, with a gap over lava
    ground =
      [ Tile c 0 Ground | c <- [0..15] ++ [40..50] ] ++
      [ Tile c (-1) Ground | c <- [0..15] ++ [40..50] ]

    -- Bridge platforms over lava (rows 0 and 1, but floating)
    bridge =
      [ Tile c 1 Brick | c <- [16..39] ] ++
      [ Tile c 0 Brick | c <- [16,19,22,25,28,31,34,37] ]  -- supports

    -- Lava floor (row -2) – visually distinct, causes death if touched
    lava = [ Tile c (-2) Ground | c <- [16..39] ]  -- we'll treat as deadly in GameState

    -- Castle walls and ceiling
    walls =
      [ Tile c r Brick | c <- [0..50], r <- [2..10] ] ++
      [ Tile 0 r Brick | r <- [0..10] ] ++
      [ Tile 50 r Brick | r <- [0..10] ]

    -- Axe at the end (column 49)
    axe = [ Tile 49 1 Axe ]

    tiles1_4 = ground ++ bridge ++ lava ++ walls ++ axe

    -- Enemies: Bowser (Koopa placeholder) and some Goombas
    enemies1_4 =
      [ Enemy (25*ts) (ts*2) (-70) 0 EAlive Koopa   -- "Bowser" (walks back and forth)
      , Enemy (20*ts) (ts*2) (-80) 0 EAlive Goomba
      , Enemy (30*ts) (ts*2) (-80) 0 EAlive Goomba
      ]

    coins1_4 = [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False) | (c,r) <-
                [(c,2) | c <- [5..10]] ++ [(20,2),(25,2),(30,2),(35,2)]]              