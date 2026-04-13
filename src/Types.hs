module Types where

import Graphics.Gloss

data TType = Ground | Brick | QBlock | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
           | SlopeLeft | SlopeRight
  deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa | Piranha deriving (Eq, Show)

data EnemyState
  = EAlive
  | EDead Float          -- timer until removal
  | EShell Float Bool    -- timer, isMoving
  | EPiranha Float Bool  -- timer, isEmerging (True = up, False = down)
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

type BB = (Float,Float,Float,Float)