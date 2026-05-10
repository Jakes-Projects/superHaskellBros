module GameState (initGS, step, handleEv) where

import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, grav, ts)
import Types
import Physics (solid, physicsMario, physicsMarioWater, mBB, hit, tBB)
import Mario (inputMario, inputMarioWater, tryJump, deathCheck)
import Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions, handleEnemyEnemyCollisions)
import PowerUp (bumpBlocks, knockPupsFromBumps, stepPup, grabPups, pickCoins, stepBrickAnims)
import Fireball (spawnFireball, stepFireball, fireballsVsEnemies)
import Level (allLevels, initMarioFromLevel)

initGS :: GS
initGS =
  let startLevel = allLevels !! 0
  in GS { gMario      = initMarioFromLevel startLevel
        , gTiles      = lTiles startLevel
        , gEnem       = lEnemies startLevel
        , gPups       = lPups startLevel
        , gCoins      = lCoins startLevel
        , gScore      = 0
        , gLives      = 3
        , gCam        = fromIntegral sW / 2
        , gKeys       = KS False False False False False

        , gPhase      = LevelIntro
        , gLevelIdx   = 0
        , gLevels     = allLevels
        , gFirebars   = lFirebars startLevel
        , gFireballs  = []
        , gTimer      = 400
        , gCoinCount  = 0
        , gBrickAnims = []
        , gPlatforms  = lPlatforms startLevel
        , gFlagOffset = 0
        , gFlagTimer  = 0
        , gDeathTimer = 0
        }

loadLevel :: Int -> GS -> GS
loadLevel idx gs
  | idx >= 0 && idx < length (gLevels gs) =
      let lvl = gLevels gs !! idx
      in gs { gMario      = initMarioFromLevel lvl
            , gTiles      = lTiles lvl
            , gEnem       = lEnemies lvl
            , gPups       = lPups lvl
            , gCoins      = lCoins lvl
            , gFirebars   = lFirebars lvl
            , gFireballs  = []
            , gCam        = fromIntegral sW / 2
            , gPhase      = LevelIntro
            , gLevelIdx   = idx
            , gTimer      = 400
            , gBrickAnims = []
            , gPlatforms  = lPlatforms lvl
            , gFlagOffset = 0
            , gFlagTimer  = 0
            , gKeys       = KS False False False False False
            , gDeathTimer = 0
            }
  | otherwise = gs

stepFirebar :: Float -> Firebar -> Firebar
stepFirebar dt fb = fb { fbAngle = fbAngle fb + fbSpeed fb * dt }

-- | Step a single moving platform: move, bounce at bounds.
stepPlatform :: Float -> MovingPlatform -> MovingPlatform
stepPlatform dt mp =
  let y1 = mpY mp + mpVY mp * dt
      vy1 = mpVY mp
      (y2, vy2)
        | y1 <= mpYMin mp = (mpYMin mp,  abs vy1)
        | y1 >= mpYMax mp = (mpYMax mp, -(abs vy1))
        | otherwise       = (y1, vy1)
  in mp { mpY = y2, mpVY = vy2 }

-- | True if Mario's horizontal range overlaps the platform.
marioOverlapsPlatformX :: Mario -> MovingPlatform -> Bool
marioOverlapsPlatformX m mp =
  let mLeft  = mX m - ts * 0.39
      mRight = mX m + ts * 0.39
      pLeft  = mpX mp
      pRight = mpX mp + fromIntegral (mpWidth mp) * ts
  in mRight > pLeft && mLeft < pRight

-- | True if Mario is riding this platform.
marioOnPlatform :: Mario -> MovingPlatform -> Bool
marioOnPlatform m mp =
  let mBot = mY m - ts * 0.5
      pTop = mpY mp + ts * 0.5
  in marioOverlapsPlatformX m mp && mBot >= pTop - 4 && mBot <= pTop + 8

-- | Snap Mario onto any platform he is touching from above.
resolvePlatforms :: [MovingPlatform] -> Mario -> Mario
resolvePlatforms plats m =
  case filter (marioOnPlatform m) plats of
    []     -> m
    (mp:_) ->
      let pTop  = mpY mp + ts * 0.5
          halfH = if mState m == Big || mState m == Fire then ts else ts * 0.5
      in m { mY = pTop + halfH, mVY = 0, mGround = True }

-- | World 2-2 is the underwater level.
isUnderwaterLevel :: Level -> Bool
isUnderwaterLevel lvl = lWorld lvl == 2 && lNumber lvl == 2

isUnderwaterGS :: GS -> Bool
isUnderwaterGS gs =
  let lvl = gLevels gs !! gLevelIdx gs
  in isUnderwaterLevel lvl

-- | Brief world screen before every level and respawn.
stepLevelIntro :: Float -> GS -> GS
stepLevelIntro dt gs =
  let t = gFlagTimer gs + dt
  in if t >= 2.0
       then gs { gPhase = Play, gFlagTimer = 0 }
       else gs { gFlagTimer = t }

step :: Float -> GS -> GS
step dt gs
  | gPhase gs == LevelIntro           = stepLevelIntro dt gs
  | gPhase gs == LevelComplete        = stepFlagAnim dt gs
  | gPhase gs == CastleComplete       = stepCastleComplete dt gs
  | gPhase gs /= Play                 = gs
  | mState (gMario gs) == MDead       = stepDeath dt gs
  | otherwise                         = gs'
  where
    ks  = gKeys gs
    sol = filter (\t -> solid (tType t) && tRow t /= (-2)) (gTiles gs)

    currentLevel = gLevels gs !! gLevelIdx gs
    underwater = isUnderwaterLevel currentLevel

    -- ── Moving platforms ──────────────────────────────────────────────────
    oldPlats = gPlatforms gs
    newPlats = map (stepPlatform dt) oldPlats

    -- Pre-carry: move Mario vertically with his platform before physics runs.
    mRiding = filter (marioOnPlatform (gMario gs)) oldPlats
    mPreMoved = case mRiding of
      []     -> gMario gs
      (mp:_) ->
        let newMp = head $ filter (\p -> abs (mpX p - mpX mp) < 1) newPlats
            dy    = mpY newMp - mpY mp
        in (gMario gs) { mY = mY (gMario gs) + dy }

    -- ── Mario movement + physics ──────────────────────────────────────────
    m0 =
      if underwater
        then inputMarioWater ks mPreMoved
        else inputMario dt ks mPreMoved

    m1 =
      if mState m0 == MDead
        then m0 { mVY = max (-900) (mVY m0 + grav * dt)
                , mY  = mY m0 + mVY m0 * dt
                }
        else
          if underwater
            then physicsMarioWater dt ks sol m0
            else physicsMario dt sol m0

    m1p =
      if underwater
        then m1
        else resolvePlatforms newPlats m1

    -- Decrement timers
    swimStroke  = underwater && kJ ks
    newSwimAnim = if swimStroke then (mSwimAnim m1p + 1) `mod` 5 else mSwimAnim m1p

    m2 = m1p { mAnim     = mAnim m1p + dt
              , mInv      = max 0 (mInv m1p - dt)
              , mFireCool = max 0 (mFireCool m1p - dt)
              , mJoeMode  = mJoeMode m1p && mState m1p == Fire
              , mSwimAnim = newSwimAnim
              , mSwimming = swimStroke
              }

    cam = max (gCam gs) (max (fromIntegral sW / 2) (mX m2))

    -- ── Enemies ──────────────────────────────────────────────────────────
    es1   = map (stepEnemy dt sol m2) (gEnem gs)
    es1'  = handleShellEnemyCollisions es1
    es1'' = handleEnemyEnemyCollisions es1'
    es2   = filter (\e -> case eState e of { EDead t -> t > 0; _ -> True }) es1''

    (m3, es3, sc1) = collideEnemies m2 es2 (gScore gs) (kJ ks)

    -- ── Collectibles ─────────────────────────────────────────────────────
    (cs, sc2) = pickCoins (mBB m3) (gCoins gs) sc1
    (ts2, pu1, sc3, newAnims, brickBroke) =
      bumpBlocks m3 (mVY m0) (gTiles gs) (gPups gs) sc2

    m3' = if brickBroke then m3 { mVY = -50 } else m3
    pu1' = knockPupsFromBumps newAnims pu1
    pu2 = map (stepPup dt (filter (solid . tType) ts2)) pu1'
    (m4, pu3, sc4) = grabPups m3' pu2 sc3

    -- ── Coin counter & 1-up ──────────────────────────────────────────────
    prevCollected  = length (filter (\(_,_,c) -> c) (gCoins gs))
    nowCollected   = length (filter (\(_,_,c) -> c) cs)
    blockCoins     = length [ () | CoinPopAnim _ _ _ _ <- newAnims ]
    newCoins       = nowCollected - prevCollected + blockCoins
    rawCoinCount   = gCoinCount gs + newCoins
    coinBonus      = rawCoinCount `div` 100
    newCoinCount   = rawCoinCount `mod` 100
    livesFromCoins = coinBonus

    -- ── Fireballs ────────────────────────────────────────────────────────
    (didShoot, fb1) =
      if kRun ks
        then spawnFireball m4 (gFireballs gs)
        else (False, gFireballs gs)

    m5 = if didShoot then m4 { mFireCool = 0.4 } else m4

    -- Bowser fires when his ft timer hits 0; spawn a straight fireball toward Mario
    bowserFireballs =
      [ Fireball
          { fiX      = eX e + (if mX m5 < eX e then 0 else ts * 2)
          , fiY      = eY e + ts
          , fiVX     = if mX m5 < eX e then -200 else 200
          , fiVY     = 0
          , fiAlive  = True
          , fiBowser = True
          }
      | e <- es3
      , eType e == Bowser
      , case eState e of { EBowser 0 _ _ _ -> True; _ -> False }
      ]

    -- Reset Bowser's fire timer after spawning so he doesn't fire every frame
    es3' = map resetBowserTimer es3
    resetBowserTimer e
      | eType e == Bowser
      , EBowser 0 jt it hp <- eState e
      = e { eState = EBowser 3.0 jt it hp }
      | otherwise = e

    fb2 = map (stepFireball dt sol) (fb1 ++ bowserFireballs)

    (fb3, es4, sc5) = fireballsVsEnemies fb2 es3' sc4

    fb4 = filter fiAlive fb3

    -- Bowser fireballs hurt Mario (downgrade state, not instant death)
    bowserFbBB fb = (fiX fb, fiY fb, 12, 12)
    touchesBowserFire = mInv m5 <= 0
      && any (\fb -> fiBowser fb && fiAlive fb && hit (mBB m5) (bowserFbBB fb)) fb4

    -- ── Firebar collision ────────────────────────────────────────────────
    firebarSegBBs =
      [ (fbX fb + dx, fbY fb + dy, ts * 0.4, ts * 0.4)
      | fb <- gFirebars gs
      , i  <- [0 .. fbLength fb - 1]
      , let spacing = ts * 0.8
            angle   = fbAngle fb
            dx = spacing * fromIntegral i * cos angle
            dy = spacing * fromIntegral i * sin angle
      ]

    touchesFirebar =
      mState m5 /= MDead
        && mInv m5 <= 0
        && any (hit (mBB m5)) firebarSegBBs

    m5'
      | touchesFirebar || touchesBowserFire
      , mState m5 == Fire  = m5 { mState = Big,   mInv = 2.0 }
      | touchesFirebar || touchesBowserFire
      , mState m5 == Big   = m5 { mState = Small, mInv = 2.0 }
      | touchesFirebar || touchesBowserFire
      = m5 { mState = MDead, mVY = 500, mVX = 0 }
      | otherwise          = m5

    -- ── Lava / timer death ───────────────────────────────────────────────
    onLava = any (\t -> tRow t == (-2) && hit (mBB m5') (tBB t)) (gTiles gs)
    timerDead = gTimer gs > 0 && (gTimer gs - dt) <= 0

    m6
      | mState m5' /= MDead && (onLava || timerDead) =
          m5' { mState = MDead, mVY = 500, mVX = 0 }
      | otherwise = m5'

    -- ── Death / lives ────────────────────────────────────────────────────
    sx = lStartX currentLevel
    sy = lStartY currentLevel

    -- livesFromCoins only (death itself is handled by stepDeath)
    livesAfter = gLives gs + livesFromCoins

    -- deathCheck no longer triggers respawn here; just pass Mario through unchanged.
    -- We keep the call signature for ph (Over/Play) but it won't fire while alive.
    (m7, ph) = deathCheck m6 livesAfter sx sy

    -- ── End conditions ───────────────────────────────────────────────────
    endX = lEndX currentLevel
    touchedAxe = any (\t -> tType t == Axe && hit (mBB m7) (tBB t)) (gTiles gs)
    bowserDead =
      any
        (\e -> eType e == Bowser && case eState e of { EDead _ -> True; EFallDead _ -> True; _ -> False })
        es4

    castleComplete = touchedAxe || bowserDead

    dropBowserIntoLava e
      | eType e == Bowser = e { eState = EFallDead 1.8, eVX = 0, eVY = 120 }
      | otherwise         = e

    es5 = if touchedAxe then map dropBowserIntoLava es4 else es4

    ph2
      | castleComplete              = CastleComplete
      | ph == Over                  = Over
      | ph == Play && mX m7 >= endX = LevelComplete
      | otherwise                   = ph

    fb_stepped = map (stepFirebar dt) (gFirebars gs)
    newTimer = max 0 (gTimer gs - dt)
    brickAnims' = stepBrickAnims dt (gBrickAnims gs ++ newAnims)

    gsTemp = gs { gMario      = m7
                , gTiles      = ts2
                , gEnem       = es5
                , gPups       = pu3
                , gCoins      = cs
                , gScore      = sc5
                , gLives      = livesAfter
                , gCam        = cam
                , gPhase      = ph2
                , gFirebars   = fb_stepped
                , gFireballs  = fb4
                , gTimer      = newTimer
                , gCoinCount  = newCoinCount
                , gBrickAnims = brickAnims'
                , gPlatforms  = newPlats
                , gFlagOffset = gFlagOffset gs
                , gFlagTimer  = if ph2 == CastleComplete then 0 else gFlagTimer gs
                , gDeathTimer = 0
                }

    gs' = gsTemp

-- | While Mario is dead: animate only his bounce, freeze everything else,
--   and wait the full 4 seconds before respawning / game-over.
stepDeath :: Float -> GS -> GS
stepDeath dt gs =
  let m           = gMario gs
      newDeathTimer = gDeathTimer gs + dt
      currentLevel  = gLevels gs !! gLevelIdx gs
      sx            = lStartX currentLevel
      sy            = lStartY currentLevel

      -- Keep animating Mario's death bounce (gravity pulls him down).
      mMoved = m { mVY = max (-900) (mVY m + grav * dt)
                 , mY  = mY m + mVY m * dt
                 }

      -- Only respawn once 4 seconds have elapsed.
      readyToRespawn = newDeathTimer >= 4.0
      livesAfter     = max 0 (gLives gs - 1)

  in if readyToRespawn
       then
         let newPhase  = if livesAfter <= 0 then Over else LevelIntro
             resetM    = m { mX = sx, mY = sy, mVX = 0, mVY = 0
                           , mState = Small, mFace = 1, mInv = 0
                           , mFireCool = 0, mCrouch = False
                           , mGround = False, mSliding = False
                           , mSkidding = False }
             respawning = newPhase == Play
         in gs { gMario      = if respawning then resetM else mMoved
               , gEnem       = if respawning then lEnemies currentLevel else gEnem gs
               , gCoins      = if respawning then lCoins   currentLevel else gCoins gs
               , gPups       = if respawning then lPups    currentLevel else gPups  gs
               , gTiles      = if respawning then lTiles   currentLevel else gTiles gs
               , gPlatforms  = if respawning then lPlatforms currentLevel else gPlatforms gs
               , gFireballs  = []
               , gBrickAnims = []
               , gCam        = if respawning then fromIntegral sW / 2 else gCam gs
               , gTimer      = if respawning then 400 else gTimer gs
               , gLives      = livesAfter
               , gPhase      = newPhase
               , gKeys       = KS False False False False False
               , gDeathTimer = 0
               }
       else gs { gMario = mMoved, gDeathTimer = newDeathTimer }

-- | Castle completion: after the axe/Bowser defeat, let Bowser visibly fall
--   before moving on. World 1-4 advances to 2-1; World 2-4 ends the game.
stepCastleComplete :: Float -> GS -> GS
stepCastleComplete dt gs =
  let currentLevel = gLevels gs !! gLevelIdx gs
      finalCastle  = lWorld currentLevel == 2 && lNumber currentLevel == 4
      t            = gFlagTimer gs + dt
      es'          = map (stepEnemy dt [] (gMario gs)) (gEnem gs)
      gs'          = gs { gEnem = es', gFlagTimer = t }
  in if t >= 2.0
       then if finalCastle
              then gs' { gPhase = Win, gFlagTimer = 0 }
              else advanceToNextLevel (gs' { gFlagTimer = 0 })
       else gs'

-- | Animate the end-of-level sequence:
--   Phase 1: flag and Mario slide DOWN the pole together (Mario faces left).
--   Phase 2: Mario lands, turns right, and walks into the castle door.
--   Advances to the next level after 7 seconds total (enough for the end song).
stepFlagAnim :: Float -> GS -> GS
stepFlagAnim dt gs =
  let totalTime  = 7.0          -- seconds for full end-of-level sequence
      -- flagStartY = 318 (flag centre at pole top, from Rendering).
      -- Flag bottom at ground surface (Y=32): flag centre = 32+18 = 50.
      -- Max travel = 318 - 50 = 268.
      poleHeight = 268             -- px the flag slides before hitting ground
      flagSpeed  = 200          -- px/s

      -- Find the flagpole column (FlagBase tile at row 0)
      poleCol    = case [ tCol t | t <- gTiles gs, tType t == FlagBase ] of
                     (c:_) -> c
                     []    -> 0
      poleWorldX = fromIntegral poleCol * ts + ts / 2

      -- Find the castle entrance X: door is centred in the 5-tile castle (col+2).
      -- Stop Mario at the centre of that tile so he's visually inside the arch.
      castleCol       = case [ tCol t | t <- gTiles gs, tType t == Castle ] of
                          cs -> if null cs then poleCol + 4 else minimum cs
      castleEntranceX = fromIntegral (castleCol + 2) * ts + ts / 2

      newOffset  = min poleHeight (gFlagOffset gs + flagSpeed * dt)
      flagDone   = newOffset >= poleHeight
      newTimer   = gFlagTimer gs + dt

      m          = gMario gs
      slideSpeed = flagSpeed

      -- Ground Y: Mario's centre Y when standing on the ground surface.
      -- Ground surface = top of row-0 tile = ts = 32.
      -- Small Mario half-height = ts/2, Big/Fire = ts. Add a few px so feet sit on top.
      groundY = case mState m of
                  Small -> ts + ts / 2 + 8    -- = 56
                  _     -> ts + ts + 8        -- = 72

      -- Phase 1: slide DOWN the pole.
      -- Y decreases toward groundY; clamp with max (stop when Y reaches groundY).
      mAfterSlide
        | not flagDone =
            let newY = max groundY (mY m - slideSpeed * dt)
            in m { mX       = poleWorldX
                 , mY       = newY
                 , mVX      = 0
                 , mVY      = 0
                 , mGround  = newY <= groundY + 1
                 , mFace    = -1    -- face left on the pole (right side of pole)
                 , mSliding = True
                 , mAnim    = mAnim m + dt
                 }
        -- Phase 2: landed — walk right into the castle door, stop there.
        | mX m < castleEntranceX =
            let newX = min castleEntranceX (mX m + 80 * dt)
            in m { mX       = newX
                 , mY       = groundY
                 , mVX      = 80
                 , mVY      = 0
                 , mGround  = True
                 , mFace    = 1
                 , mSliding = False
                 , mAnim    = mAnim m + dt   -- drives the run animation cycle
                 }
        -- Phase 3: inside the castle — stand still and wait for song to finish.
        | otherwise =
            m { mVX      = 0
              , mGround  = True
              , mFace    = 1
              , mSliding = False
              }

  in if newTimer >= totalTime
       then advanceToNextLevel gs
       else gs { gFlagOffset = newOffset
               , gFlagTimer  = newTimer
               , gMario      = mAfterSlide
               }

initMarioForNextLevel :: Level -> Mario -> Mario
initMarioForNextLevel lvl oldMario =
  let base = initMarioFromLevel lvl
      keptState = case mState oldMario of
                    MDead -> Small
                    st    -> st
  in base { mState = keptState
          , mJoeMode = mJoeMode oldMario && keptState == Fire
          }

advanceToNextLevel :: GS -> GS
advanceToNextLevel gs =
  let nextIdx = gLevelIdx gs + 1
  in if nextIdx < length (gLevels gs)
       then
         let nextLvl = gLevels gs !! nextIdx
         in gs { gMario      = initMarioForNextLevel nextLvl (gMario gs)
               , gTiles      = lTiles nextLvl
               , gEnem       = lEnemies nextLvl
               , gPups       = lPups nextLvl
               , gCoins      = lCoins nextLvl
               , gFirebars   = lFirebars nextLvl
               , gFireballs  = []
               , gCam        = fromIntegral sW / 2
               , gPhase      = LevelIntro
               , gLevelIdx   = nextIdx
               , gTimer      = 400
               , gBrickAnims = []
               , gPlatforms  = lPlatforms nextLvl
               , gFlagOffset = 0
               , gFlagTimer  = 0
               , gKeys       = KS False False False False False
               , gDeathTimer = 0
               }
       else gs { gPhase = Win }

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv (EventKey (Char d) Down _ _) gs
  | d >= '1' && d <= '8' = loadLevel (fromEnum d - fromEnum '1') gs
handleEv _ gs | gPhase gs /= Play = gs
handleEv ev gs = case ev of
  EventKey k Down _ _ ->
    let m0 = joeCheck k (bufferKey k (gMario gs))
        m1 =
          if isUnderwaterGS gs
            then m0
            else tryJump' k m0
    in gs { gMario = m1
          , gKeys  = setK k True (gKeys gs)
          }

  EventKey k Up _ _ ->
    gs { gKeys = setK k False (gKeys gs) }

  _ -> gs
  where
    -- Append letter to the buffer, keep last 3 chars only.
    bufferKey (Char c) m | c `elem` ("joe" :: String) =
      let buf' = drop (max 0 (length (mJoeBuffer m) + 1 - 3)) (mJoeBuffer m ++ [c])
      in m { mJoeBuffer = buf' }
    bufferKey _ m = m

    -- Toggle joe mode if buffer == "joe" and Mario is Fire.
    joeCheck (Char _) m
      | mJoeBuffer m == "joe" && mState m == Fire =
          m { mJoeMode = not (mJoeMode m), mJoeBuffer = "" }
      | mJoeBuffer m == "joe" =
          m { mJoeBuffer = "" }
    joeCheck _ m = m

    setK (Char 'a')            v k = k { kL   = v }
    setK (Char 'd')            v k = k { kR   = v }
    setK (Char 'w')            v k = k { kJ   = v }
    setK (Char 's')            v k = k { kD   = v }
    setK (SpecialKey KeyLeft)  v k = k { kL   = v }
    setK (SpecialKey KeyRight) v k = k { kR   = v }
    setK (SpecialKey KeySpace) v k = k { kJ   = v }
    setK (SpecialKey KeyUp)    v k = k { kJ   = v }
    setK (SpecialKey KeyDown)  v k = k { kD   = v }
    setK (Char 'z')            v k = k { kRun = v }
    setK (Char 'x')            v k = k { kRun = v }
    setK _ _ k = k

    tryJump' (SpecialKey KeySpace) m = tryJump m
    tryJump' (SpecialKey KeyUp)    m = tryJump m
    tryJump' (Char 'w')            m = tryJump m
    tryJump' _                     m = m