module Types where

import Graphics.Gloss

data TType = Ground | Brick | QBlock | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
           | SlopeLeft | SlopeRight | Axe | FirebarTile | Step
           deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa | Piranha | Bowser deriving (Eq, Show)

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

-- The type of item a power-up block contains.
data PUpType = Mushroom | FireFlower | Star deriving (Eq, Show)

data PUp = PUp
  { pX, pY, pVY :: Float
  , pAlive :: Bool
  , pType  :: PUpType
  } deriving Show

-- Mario's power state: Small < Big < Fire.  MDead = death animation.
-- Damage steps down exactly one level: Fire -> Big -> Small -> MDead.
data MS = Small | Big | Fire | MDead deriving (Eq, Show)

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
  { gMario     :: Mario
  , gTiles     :: [Tile]
  , gEnem      :: [Enemy]
  , gPups      :: [PUp]
  , gCoins     :: [(Float,Float,Bool)]
  , gScore     :: Int
  , gLives     :: Int
  , gCam       :: Float
  , gKeys      :: KS
  , gPhase     :: Phase
  , gLevelIdx  :: Int
  , gLevels    :: [Level]
  , gFirebars  :: [Firebar]
  , gTimer     :: Float
  , gCoinCount :: Int   -- running coin total; every 100 grants a 1-up
  } deriving Show

type BB = (Float,Float,Float,Float)