module Level (initMario, mkTiles, mkEnemies, mkCoins, initKS) where

import Constants (ts)
import Types

initMario :: Mario
initMario = Mario (ts*3) (ts*1.5) 0 0 False Small 1 0 0

initKS :: KS
initKS = KS False False False False

mkTiles :: [Tile]
mkTiles = ground ++ structs ++ pipes ++ stairs ++ flag ++ castle
  where
    ground = [Tile c r Ground | c <- [0..211], r <- [0,(-1)]]

    structs =
      [ Tile 16 3 QBlock
      , Tile 20 3 Brick
      , Tile 21 3 QBlock
      , Tile 22 3 Brick
      , Tile 23 3 QBlock
      , Tile 24 3 Brick
      , Tile 22 7 QBlock
      , Tile 78 3 Brick
      , Tile 79 3 QBlock
      , Tile 80 3 Brick
      , Tile 81 3 Brick
      , Tile 82 3 QBlock
      , Tile 83 3 Brick
      , Tile 78 7 QBlock
      , Tile 79 7 QBlock
      , Tile 80 7 QBlock
      , Tile 81 7 Brick
      , Tile 91 3 Brick
      , Tile 92 3 Brick
      , Tile 93 3 QBlock
      , Tile 94 3 Brick
      , Tile 95 3 Brick
      , Tile 100 3 Brick
      , Tile 101 3 QBlock
      , Tile 102 3 Brick
      , Tile 107 3 Brick
      , Tile 108 3 Brick
      , Tile 109 3 Brick
      , Tile 107 7 Brick
      , Tile 108 7 QBlock
      , Tile 109 7 Brick
      , Tile 110 7 Brick
      ]

    pipes = concatMap mkPipe
      [ (28, 2), (38, 3), (46, 4), (57, 4), (163, 2) ]

    mkPipe (col, height) =
      [Tile col r t | (r,t) <- zip [1..height] (replicate (height-1) Pipe ++ [PipeTop])]
      ++ [Tile (col+1) r PipeR | r <- [1..height]]

    stairs =
      stairU 127 4 ++ stairD 133 4 ++
      stairU 140 4 ++ stairD 146 4 ++
      [Tile (196+i) r Ground | i <- [0..7], r <- [1..i+1]]

    stairU c h = [Tile (c+i) r Ground | i <- [0..h-1], r <- [1..i+1]]
    stairD c h = [Tile (c+i) r Ground | i <- [0..h-1], r <- [1..(h-i)]]

    flag = [Tile 204 r FlagPole | r <- [1..10]] ++ [Tile 204 0 FlagBase]

    castle =
      [ Tile c r Castle | c <- [207..211], r <- [0..4] ]
      ++ [ Tile c 5 Castle | c <- [207,209,211] ]

mkEnemies :: [Enemy]
mkEnemies = map mkG gPs ++ map mkK kPs
  where
    mkG c = Enemy (c*ts) ts (-80) True False 0 Goomba
    mkK c = Enemy (c*ts) ts (-70) True False 0 Koopa
    gPs = [20,22,37,40,57,59,80,82,100,102,110,116,150,152]
    kPs = [60,92,130]

mkCoins :: [(Float,Float,Bool)]
mkCoins =
  [(fromIntegral c*ts + ts/2, fromIntegral r*ts + ts/2, False)
  | (c,r) <-
    [(c,2) | c <- [1..4]]
    ++ [(21,5),(23,5),(79,5),(82,5),(93,5),(101,5),(108,9)]
  ]