module Types where

import Graphics.Gloss

data TType = Ground | Brick | QBlock | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
           | SlopeLeft | SlopeRight | Axe | FirebarTile
           deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa | Piranha deriving (Eq, Show)

data EnemyState
  = EAlive
  | EDead Float
  | EShell Float Bool
  | EPiranha Float Bool
  deriving (Eq, Show)

data Enemy = Enemy
  { eX, eY, eVX, eVY :: Float
  , eState :: EnemyState
  , eType  :: EType
  } deriving Show

data PUp = PUp { pX, pY, pVY :: Float, pAlive :: Bool } deriving Show

data MS = Small | Big | MDead deriving (Eq, Show)

data Mario = Mario
  { mX, mY   :: Float
  , mVX, mVY :: Float
  , mGround  :: Bool
  , mState   :: MS
  , mFace    :: Int
  , mAnim    :: Float
  , mInv     :: Float
  } deriving Show

data KS = KS { kL, kR, kJ, kRun :: Bool } deriving Show

data Phase = Play | Over | Win | LevelComplete deriving (Eq, Show)

data Level = Level
  { lTiles    :: [Tile]
  , lEnemies  :: [Enemy]
  , lCoins    :: [(Float,Float,Bool)]
  , lPups     :: [PUp]
  , lFirebars :: [Firebar]
  , lStartX   :: Float
  , lStartY   :: Float
  , lEndX     :: Float
  , lWorld    :: Int
  , lNumber   :: Int
  } deriving Show

data Firebar = Firebar
  { fbX, fbY :: Float
  , fbAngle  :: Float
  , fbSpeed  :: Float
  , fbLength :: Int
  } deriving Show

data GS = GS
  { gMario    :: Mario
  , gTiles    :: [Tile]
  , gEnem     :: [Enemy]
  , gPups     :: [PUp]
  , gCoins    :: [(Float,Float,Bool)]
  , gScore    :: Int
  , gLives    :: Int
  , gCam      :: Float
  , gKeys     :: KS
  , gPhase    :: Phase
  , gLevelIdx :: Int
  , gLevels   :: [Level]
  , gFirebars :: [Firebar]
  } deriving Show

type BB = (Float,Float,Float,Float)