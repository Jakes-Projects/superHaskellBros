module Types where

import Graphics.Gloss

data TType = Ground | Brick | QBlock | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
           | SlopeLeft | SlopeRight   -- 45° slopes
  deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa deriving (Eq, Show)

data Enemy = Enemy
  { eX, eY, eVX, eVY :: Float   -- added eVY
  , eAlive :: Bool
  , eDead  :: Bool
  , eTimer :: Float
  , eType  :: EType
  } deriving Show

data PUp = PUp { pX, pY, pVY :: Float, pAlive :: Bool } deriving Show

data MS = Small | Big | MDead deriving (Eq, Show)

data Mario = Mario
  { mX, mY   :: Float
  , mVX, mVY :: Float
  , mGround  :: Bool
  , mState   :: MS
  , mFace    :: Int     -- 1=right -1=left
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

type BB = (Float,Float,Float,Float)  -- cx cy w h