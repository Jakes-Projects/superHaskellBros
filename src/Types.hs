module Types where

import Graphics.Gloss

data QContent = QCoin | QPowerUp deriving (Eq, Show)

data TType = Ground | Brick | QBlock QContent | Used
           | Pipe | PipeTop | PipeR | FlagPole | FlagBase | Castle
           | SlopeLeft | SlopeRight | Axe | FirebarTile | Step | Coral
           deriving (Eq, Show)

data Tile = Tile { tCol :: Int, tRow :: Int, tType :: TType } deriving Show

data EType = Goomba | Koopa | Piranha | Bowser | CheepCheep | GreenCheep | JumpingCheep | Blooper
  deriving (Eq, Show)

data EnemyState
  = EAlive
  | EDead Float
  | EFallDead Float
      -- enemy has been knocked upward and is falling off the map
  | EShell Float Bool
  | EPiranha Float Bool
  | EBowser Float Float Float Int
      -- fireTimer, jumpTimer, idleTimer, hitPoints
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
  , mJoeMode    :: Bool    -- True when "joe" skin is active (Fire only)
  , mJoeBuffer  :: String  -- tracks recent keypresses for "joe" detection
  , mSwimAnim   :: Int     -- swim animation frame (0-4), advances each stroke
  , mSwimming   :: Bool    -- True when swim button is held
  , mSliding         :: Bool    -- True while sliding down the end-of-level flagpole
  , mSkidding        :: Bool    -- True while braking hard (opposite direction input)
  , mTransformTimer  :: Float   -- counts down during power-up flash (0 = not transforming)
  , mTransformTarget :: MS      -- the state to transition INTO after the flash
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

data Phase = LevelIntro | Play | Over | Win | LevelComplete | CastleComplete | PipeEntry deriving (Eq, Show)

-- | A transient block animation.
--   BumpAnim  col row timer  — block bounces up (timer counts down from ~0.12s)
--   BreakAnim col row timer  — broken-sprite flash then 4 debris particles
--   CoinAnim  x   y   vy timer — coin flies up out of a ? block
data BrickAnim
  = BumpAnim  Int   Int   Float          -- col, row, timeLeft
  | BreakAnim Int   Int   Float          -- col, row, timeLeft (0.15s total)
  | CoinPopAnim Float Float Float Float  -- x, y, vy, timeLeft
  deriving Show

-- | A moving platform (lift/elevator).
--   Travels vertically between yMin and yMax at the given speed.
--   width is in tiles (e.g. 3 = 3 tiles wide).
data MovingPlatform = MovingPlatform
  { mpX     :: Float   -- world X (left edge)
  , mpY     :: Float   -- world Y (top surface, like row * ts)
  , mpVY    :: Float   -- current vertical velocity (positive = up)
  , mpYMin  :: Float   -- lower bound (Y at bottom of travel)
  , mpYMax  :: Float   -- upper bound (Y at top of travel)
  , mpWidth :: Int     -- width in tiles
  } deriving Show

data Level = Level
  { lTiles     :: [Tile]
  , lEnemies   :: [Enemy]
  , lCoins     :: [(Float,Float,Bool)]
  , lPups      :: [PUp]
  , lFirebars  :: [Firebar]
  , lPlatforms :: [MovingPlatform]
  , lStartX    :: Float
  , lStartY    :: Float
  , lEndX      :: Float
  , lWorld     :: Int
  , lNumber    :: Int
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
  , gFireballs  :: [Fireball]
  , gTimer      :: Float
  , gCoinCount  :: Int
  , gBrickAnims :: [BrickAnim]
  , gPlatforms  :: [MovingPlatform]
  , gFlagOffset :: Float   -- how far the flag has slid down the pole (0 = top)
  , gFlagTimer  :: Float   -- countdown after flag fully lowered before advancing
  , gDeathTimer :: Float   -- time elapsed since Mario died (for death jingle)
  , gFreezeTimer :: Float  -- world is frozen while Mario transforms (>0 = frozen)
  , gPipeTimer  :: Float   -- counts up during PipeEntry cutscene
  } deriving Show

type BB = (Float,Float,Float,Float)