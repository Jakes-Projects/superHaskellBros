module Types where

import Graphics.Gloss

data QContent = QCoin | QPowerUp deriving (Eq, Show)

data TType = Ground | Brick | QBlock QContent | Used
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
  | EBowser Float Float Float   -- fireTimer, jumpTimer, idleTimer (counts down from 5)
  deriving (Eq, Show)

data Enemy = Enemy
  { eX, eY, eVX, eVY :: Float
  , eState :: EnemyState
  , eType  :: EType
  } deriving Show

data PUpType = Mushroom | FireFlower | Star deriving (Eq, Show)

data PUp = PUp
  { pX, pY, pVX, pVY :: Float
  , pAlive :: Bool
  , pType  :: PUpType
  } deriving Show

data MS = Small | Big | Fire | MDead deriving (Eq, Show)

data Mario = Mario
  { mX, mY      :: Float
  , mVX, mVY    :: Float
  , mGround     :: Bool
  , mState      :: MS
  , mFace       :: Int
  , mAnim       :: Float
  , mInv        :: Float
  , mFireCool   :: Float   -- seconds until next fireball can be shot
  , mCrouch     :: Bool    -- crouching (Big/Fire only, grounded, down held)
  } deriving Show

-- | A fireball shot by Fire Mario.
--   Travels horizontally, bounces off the ground, destroyed by walls.
data Fireball = Fireball
  { fiX, fiY   :: Float
  , fiVX, fiVY :: Float
  , fiAlive    :: Bool
  , fiBowser   :: Bool   -- True = Bowser's fire (straight, no bounce)
  } deriving Show

data KS = KS { kL, kR, kJ, kRun, kD :: Bool } deriving Show

data Phase = Play | Over | Win | LevelComplete deriving (Eq, Show)

-- | A transient block animation.
--   BumpAnim  col row timer  — block bounces up (timer counts down from ~0.12s)
--   BreakAnim col row timer  — broken-sprite flash then 4 debris particles
--   CoinAnim  x   y   vy timer — coin flies up out of a ? block
data BrickAnim
  = BumpAnim  Int   Int   Float          -- col, row, timeLeft
  | BreakAnim Int   Int   Float          -- col, row, timeLeft (0.15s total)
  | CoinPopAnim Float Float Float Float  -- x, y, vy, timeLeft
  deriving Show

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
  { gMario      :: Mario
  , gTiles      :: [Tile]
  , gEnem       :: [Enemy]
  , gPups       :: [PUp]
  , gCoins      :: [(Float,Float,Bool)]
  , gScore      :: Int
  , gLives      :: Int
  , gCam        :: Float
  , gKeys       :: KS
  , gPhase      :: Phase
  , gLevelIdx   :: Int
  , gLevels     :: [Level]
  , gFirebars   :: [Firebar]
  , gFireballs  :: [Fireball]   -- Mario's active fireballs
  , gTimer      :: Float
  , gCoinCount  :: Int
  , gBrickAnims :: [BrickAnim]
  } deriving Show

type BB = (Float,Float,Float,Float)