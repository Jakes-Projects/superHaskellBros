module Sound
  ( SoundEvent(..)
  , playSfx
  , playSfxHandle
  , detectSoundEvents
  , jumpSfx
  ) where

import System.Process (spawnProcess, ProcessHandle)
import System.IO.Error (catchIOError)
import Types

-- ---------------------------------------------------------------------------
-- Sound event sum type
-- ---------------------------------------------------------------------------

data SoundEvent
  = SfxJumpSmall       -- Small Mario jumps
  | SfxJumpBig         -- Super Mario / Joe (Fire) jumps
  | SfxStomp           -- Mario stomps an enemy / fireball kill
  | SfxKickShell       -- Mario kicks a stationary Koopa shell
  | SfxPowerUpTaken    -- Mario grabs a Mushroom or Fire Flower
  | SfxPowerUpAppears  -- A Mushroom or Fire Flower is revealed from a ? block
  | SfxOneUp           -- Mario earns an extra life
  | SfxFlagpole        -- Mario touches the end-of-level flagpole
  | SfxPipeTravelDown  -- Mario is hit / shrinks
  | SfxGameOver        -- All lives lost
  | SfxFireball        -- Mario shoots a fireball
  | SfxBowserFire      -- Bowser shoots a fireball
  | SfxCoin            -- A coin is collected (floor coin or block coin)
  | SfxBlockBump       -- Small Mario bumps a brick/block without breaking it
  | SfxBrickBreak      -- Big/Fire/Joe Mario shatters a brick
  | SfxBowserFall      -- Bowser falls into lava
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Playback
-- ---------------------------------------------------------------------------

sfxPath :: SoundEvent -> FilePath
sfxPath SfxJumpSmall      = "assets/jump_small.wav"
sfxPath SfxJumpBig        = "assets/jump_big.wav"
sfxPath SfxStomp          = "assets/stomp.wav"
sfxPath SfxKickShell      = "assets/kick_shell.wav"
sfxPath SfxPowerUpTaken   = "assets/power_up_taken.wav"
sfxPath SfxPowerUpAppears = "assets/power_up_appears.wav"
sfxPath SfxOneUp          = "assets/1_up.wav"
sfxPath SfxFlagpole       = "assets/flagpole.wav"
sfxPath SfxPipeTravelDown = "assets/pipe_travel_power_down.wav"
sfxPath SfxGameOver       = "assets/game_over.wav"
sfxPath SfxFireball       = "assets/fireball.wav"
sfxPath SfxBowserFire     = "assets/bowser_fire.wav"
sfxPath SfxCoin           = "assets/coin.wav"
sfxPath SfxBlockBump      = "assets/block_bump.wav"
sfxPath SfxBrickBreak     = "assets/brick_break.wav"
sfxPath SfxBowserFall     = "assets/bowser_fall.wav"

-- | Fire-and-forget playback; errors silently swallowed.
playSfx :: SoundEvent -> IO ()
playSfx ev = playSfxHandle ev >> return ()

-- | Like 'playSfx' but returns the ProcessHandle so the caller can wait on it.
playSfxHandle :: SoundEvent -> IO (Maybe ProcessHandle)
playSfxHandle ev =
  catchIOError
    (Just <$> spawnProcess "afplay" [sfxPath ev])
    (\_ -> return Nothing)

-- ---------------------------------------------------------------------------
-- Event detection  (pure, called once per frame with old and new GS)
-- ---------------------------------------------------------------------------

detectSoundEvents :: GS -> GS -> [SoundEvent]
detectSoundEvents old new = concat
  [ detectGameOver       old new
  , detectFlagpole       old new
  , detectPipeDown       old new
  , detectBowserFall     old new
  , detectStomp          old new
  , detectKickShell      old new
  , detectPowerUpAppears old new
  , detectPowerUp        old new
  , detectOneUp          old new
  , detectFireball       old new
  , detectBowserFire     old new
  , detectCoin           old new
  , detectBlockBump      old new
  , detectBrickBreak     old new
  ]

-- ---------------------------------------------------------------------------
-- Individual detectors
-- ---------------------------------------------------------------------------

detectGameOver :: GS -> GS -> [SoundEvent]
detectGameOver old new
  | gPhase old /= Over && gPhase new == Over = [SfxGameOver]
  | otherwise                                = []

detectFlagpole :: GS -> GS -> [SoundEvent]
detectFlagpole old new
  | gPhase old /= LevelComplete && gPhase new == LevelComplete = [SfxFlagpole]
  | otherwise                                                   = []

detectPipeDown :: GS -> GS -> [SoundEvent]
detectPipeDown old new =
  let mOld = gMario old
      mNew  = gMario new
      downgraded =
           (mState mOld == Fire  && mState mNew == Big)
        || (mState mOld == Big   && mState mNew == Small)
        || (mState mOld /= MDead && mState mNew == MDead)
  in if downgraded && mInv mNew > 0 then [SfxPipeTravelDown] else []

-- | Bowser fall: a Bowser enemy just transitioned to EFallDead.
detectBowserFall :: GS -> GS -> [SoundEvent]
detectBowserFall old new =
  let pairs = zip (gEnem old) (gEnem new)
      justFell (eo, en) =
        eType eo == Bowser
        && (case eState eo of { EFallDead _ -> False; _ -> True })
        && (case eState en of { EFallDead _ -> True;  _ -> False })
  in if any justFell pairs then [SfxBowserFall] else []

-- | Stomp: any active enemy (including Piranha) killed or Koopa pushed into shell.
detectStomp :: GS -> GS -> [SoundEvent]
detectStomp old new =
  let pairs = zip (gEnem old) (gEnem new)
      isActive e = case eState e of
        EAlive          -> True
        EBowser _ _ _ _ -> True
        EShell _ _      -> True
        EPiranha _ _    -> True
        _               -> False
      stompEvent (eo, en)
        | isActive eo, EDead     _ <- eState en = True
        | isActive eo, EFallDead _ <- eState en = True
        -- Koopa stomped into its shell for the first time
        | EAlive         <- eState eo
        , EShell _ False <- eState en           = True
        -- Moving shell stopped by a stomp
        | EShell _ True  <- eState eo
        , EShell _ False <- eState en           = True
        | otherwise                             = False
  in if any stompEvent pairs then [SfxStomp] else []

detectKickShell :: GS -> GS -> [SoundEvent]
detectKickShell old new =
  let pairs = zip (gEnem old) (gEnem new)
      kicked (eo, en) = case (eState eo, eState en) of
        (EShell _ False, EShell _ True) -> True
        _                               -> False
  in if any kicked pairs then [SfxKickShell] else []

-- | Power-up appears: a new PUp entry exists in gPups that wasn't there before.
--   This fires the moment the mushroom/fire flower pops out of the block.
detectPowerUpAppears :: GS -> GS -> [SoundEvent]
detectPowerUpAppears old new
  | length (gPups new) > length (gPups old) = [SfxPowerUpAppears]
  | otherwise                               = []

-- | Power-up collected: Mario's transform timer just started (the moment he
--   touches the power-up), so the sound fires immediately rather than waiting
--   for the flash to finish.
detectPowerUp :: GS -> GS -> [SoundEvent]
detectPowerUp old new =
  let mOld = gMario old
      mNew  = gMario new
      justStarted = mTransformTimer mOld <= 0 && mTransformTimer mNew > 0
  in if justStarted then [SfxPowerUpTaken] else []

detectOneUp :: GS -> GS -> [SoundEvent]
detectOneUp old new
  | gLives new > gLives old = [SfxOneUp]
  | otherwise               = []

-- | Mario's fireball: a new non-Bowser fireball appeared in gFireballs.
detectFireball :: GS -> GS -> [SoundEvent]
detectFireball old new =
  let newFbs = filter (not . fiBowser) (gFireballs new)
      oldFbs = filter (not . fiBowser) (gFireballs old)
  in if length newFbs > length oldFbs then [SfxFireball] else []

-- | Bowser's fireball: a new Bowser fireball appeared in gFireballs.
detectBowserFire :: GS -> GS -> [SoundEvent]
detectBowserFire old new =
  let newFbs = filter fiBowser (gFireballs new)
      oldFbs = filter fiBowser (gFireballs old)
  in if length newFbs > length oldFbs then [SfxBowserFire] else []

-- | Coin collected: either a floor coin was grabbed or a CoinPopAnim was spawned
--   (block coin popped). One SfxCoin fires per coin collected this frame.
detectCoin :: GS -> GS -> [SoundEvent]
detectCoin old new =
  let floorCoins  = length (filter (\(_,_,c) -> c) (gCoins new))
                  - length (filter (\(_,_,c) -> c) (gCoins old))
      blockCoins  = length [ () | CoinPopAnim {} <- gBrickAnims new ]
                  - length [ () | CoinPopAnim {} <- gBrickAnims old ]
      total       = max 0 floorCoins + max 0 blockCoins
  in replicate total SfxCoin

-- | Block bump: a BumpAnim just appeared without a BreakAnim on the same frame.
--   This means small Mario (or any Mario on a non-breaking block) bumped a tile.
detectBlockBump :: GS -> GS -> [SoundEvent]
detectBlockBump old new =
  let newBumps   = length [ () | BumpAnim  {} <- gBrickAnims new ]
      oldBumps   = length [ () | BumpAnim  {} <- gBrickAnims old ]
      newBreaks  = length [ () | BreakAnim {} <- gBrickAnims new ]
      oldBreaks  = length [ () | BreakAnim {} <- gBrickAnims old ]
      bumpAdded  = newBumps  > oldBumps
      breakAdded = newBreaks > oldBreaks
  -- Only fire the bump sound if no brick was broken this frame
  in if bumpAdded && not breakAdded then [SfxBlockBump] else []

-- | Brick break: a BreakAnim just appeared (big/fire/joe Mario smashed a brick).
detectBrickBreak :: GS -> GS -> [SoundEvent]
detectBrickBreak old new =
  let newBreaks = length [ () | BreakAnim {} <- gBrickAnims new ]
      oldBreaks = length [ () | BreakAnim {} <- gBrickAnims old ]
  in if newBreaks > oldBreaks then [SfxBrickBreak] else []

-- ---------------------------------------------------------------------------
-- Jump SFX helper  (called from handleEvIO in Main)
-- ---------------------------------------------------------------------------

jumpSfx :: Mario -> Mario -> Maybe SoundEvent
jumpSfx before after
  | mVY after /= mVY before =
      Just $ case mState before of
        Small -> SfxJumpSmall
        _     -> SfxJumpBig
  | otherwise = Nothing
