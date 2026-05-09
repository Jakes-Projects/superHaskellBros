module GameState (initGS, step, handleEv) where

import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, grav, ts)
import Types
import Physics (solid, physicsMario, mBB, hit, tBB)
import Mario (inputMario, tryJump, deathCheck)
import Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions, handleEnemyEnemyCollisions)
import PowerUp (bumpBlocks, stepPup, grabPups, pickCoins, stepBrickAnims)
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

        , gPhase      = Play
        , gLevelIdx   = 0
        , gLevels     = allLevels
        , gFirebars   = lFirebars startLevel
        , gFireballs  = []
        , gTimer      = 400
        , gCoinCount  = 0
        , gBrickAnims = []
        }

loadLevel :: Int -> GS -> GS
loadLevel idx gs
  | idx >= 0 && idx < length (gLevels gs) =
      let lvl = gLevels gs !! idx
      in gs { gMario     = initMarioFromLevel lvl
            , gTiles     = lTiles lvl
            , gEnem      = lEnemies lvl
            , gPups      = lPups lvl
            , gCoins     = lCoins lvl
            , gFirebars  = lFirebars lvl
            , gFireballs = []
            , gCam       = fromIntegral sW / 2
            , gPhase     = Play
            , gLevelIdx  = idx
            , gTimer     = 400
            , gBrickAnims = []
            }
  | otherwise = gs

stepFirebar :: Float -> Firebar -> Firebar
stepFirebar dt fb = fb { fbAngle = fbAngle fb + fbSpeed fb * dt }

step :: Float -> GS -> GS
step dt gs
  | gPhase gs /= Play = gs
  | otherwise = gs'
  where
    ks  = gKeys gs
    sol = filter (solid . tType) (gTiles gs)

    currentLevel = gLevels gs !! gLevelIdx gs

    -- ── Mario movement + physics ─────────────────────────────────────────
    m0 = inputMario ks (gMario gs)
    m1 = if mState m0 == MDead
           then m0 { mVY = max (-900) (mVY m0 + grav * dt)
                   , mY  = mY m0 + mVY m0 * dt }
           else physicsMario dt sol m0
    -- Decrement timers (animation, invincibility, fire cooldown)
    m2 = m1 { mAnim     = mAnim m1 + dt
             , mInv      = max 0 (mInv m1 - dt)
             , mFireCool = max 0 (mFireCool m1 - dt)
             -- If Mario lost Fire state, clear Joe mode
             , mJoeMode  = mJoeMode m1 && mState m1 == Fire
             }

    cam = max (gCam gs) (max (fromIntegral sW / 2) (mX m2))

    -- ── Enemies ──────────────────────────────────────────────────────────
    es1  = map (stepEnemy dt sol m2) (gEnem gs)
    es1' = handleShellEnemyCollisions es1
    es1'' = handleEnemyEnemyCollisions es1'
    es2  = filter (\e -> case eState e of { EDead t -> t > 0; _ -> True }) es1''

    (m3, es3, sc1) = collideEnemies m2 es2 (gScore gs) (kJ ks)

    -- ── Collectibles ─────────────────────────────────────────────────────
    (cs,  sc2)       = pickCoins (mBB m3) (gCoins gs) sc1
    (ts2, pu1, sc3, newAnims, brickBroke) = bumpBlocks m3 (mVY m0) (gTiles gs) (gPups gs) sc2
    -- When Big/Fire Mario breaks a brick, kill upward velocity so he bounces back down
    -- (same ceiling-hit behaviour the physics engine applies to unbreakable blocks)
    m3' = if brickBroke then m3 { mVY = -50 } else m3
    pu2 = map (stepPup dt (filter (solid . tType) ts2)) pu1
    (m4, pu3, sc4) = grabPups m3' pu2 sc3

    -- ── Coin counter & 1-up ──────────────────────────────────────────────
    prevCollected  = length (filter (\(_,_,c) -> c) (gCoins gs))
    nowCollected   = length (filter (\(_,_,c) -> c) cs)
    newCoins       = nowCollected - prevCollected
    rawCoinCount   = gCoinCount gs + newCoins
    coinBonus      = rawCoinCount `div` 100
    newCoinCount   = rawCoinCount `mod` 100
    livesFromCoins = coinBonus

    -- ── Fireballs ────────────────────────────────────────────────────────
    -- Spawn only when the run/fire button (Z or X) is held — same as the
    -- NES B button which handled both running and shooting.
    (didShoot, fb1) = if kRun ks then spawnFireball m4 (gFireballs gs)
                                  else (False, gFireballs gs)
    m5 = if didShoot then m4 { mFireCool = 0.4 } else m4

    -- Step existing fireballs (physics + wall kill)
    fb2 = map (stepFireball dt sol) fb1

    -- Fireball vs enemy collisions
    (fb3, es4, sc5) = fireballsVsEnemies fb2 es3 sc4

    -- Remove dead fireballs (keep alive ones only)
    fb4 = filter fiAlive fb3

    -- ── Firebar collision ────────────────────────────────────────────────
    -- Check each segment of every firebar against Mario's bounding box.
    -- Uses the same position math as Rendering.drawFirebar.
    firebarSegBBs = [ (fbX fb + dx, fbY fb + dy, ts * 0.4, ts * 0.4)
                    | fb <- gFirebars gs
                    , i  <- [0 .. fbLength fb - 1]
                    , let spacing = ts * 0.8
                          angle   = fbAngle fb
                          dx = spacing * fromIntegral i * cos angle
                          dy = spacing * fromIntegral i * sin angle
                    ]
    touchesFirebar = mState m5 /= MDead
                  && mInv m5 <= 0
                  && any (hit (mBB m5)) firebarSegBBs

    -- Power-down chain: Fire → Big → Small → MDead (same as hurtMario in Enemy.hs)
    m5' | not touchesFirebar = m5
        | mState m5 == Fire  = m5 { mState = Big,   mInv = 2.0 }
        | mState m5 == Big   = m5 { mState = Small, mInv = 2.0 }
        | otherwise          = m5 { mState = MDead, mVY = 500, mVX = 0 }

    -- ── Lava / timer death ───────────────────────────────────────────────
    onLava    = any (\t -> tRow t == (-2) && hit (mBB m5') (tBB t)) (gTiles gs)
    timerDead = gTimer gs > 0 && (gTimer gs - dt) <= 0

    -- Guard: only trigger on a living Mario (prevents mVY reset bounce loop)
    m6 | mState m5' /= MDead && (onLava || timerDead) =
             m5' { mState = MDead, mVY = 500, mVX = 0 }
       | otherwise = m5'

    -- ── Death / lives ────────────────────────────────────────────────────
    sx = lStartX currentLevel
    sy = lStartY currentLevel

    marioDied  = mState m6 == MDead && mY m6 < -300
    livesAfter = max 0 (gLives gs + livesFromCoins - (if marioDied then 1 else 0))

    (m7, ph) = deathCheck m6 livesAfter sx sy

    -- ── End conditions ───────────────────────────────────────────────────
    endX        = lEndX currentLevel
    touchedAxe  = any (\t -> tType t == Axe && hit (mBB m7) (tBB t)) (gTiles gs)
    bowserDead  = any (\e -> eType e == Bowser && case eState e of { EDead _ -> True; _ -> False }) es4

    ph2 | touchedAxe || bowserDead = Win
        | ph == Over                  = Over
        | ph == Play && mX m7 >= endX = LevelComplete
        | otherwise                   = ph

    fb_stepped   = map (stepFirebar dt) (gFirebars gs)
    newTimer     = max 0 (gTimer gs - dt)
    -- Step brick/block animations, adding any new ones from this frame
    brickAnims'  = stepBrickAnims dt (gBrickAnims gs ++ newAnims)

    -- ── Respawn reset ────────────────────────────────────────────────────
    respawning    = marioDied && ph2 == Play
    activeEnem    = if respawning then lEnemies currentLevel else es4
    activeCoins   = if respawning then lCoins   currentLevel else cs
    activePups    = if respawning then lPups    currentLevel else pu3
    activeTiles   = if respawning then lTiles   currentLevel else ts2
    activeTimer   = if respawning then 400                   else newTimer
    activeCam     = if respawning then fromIntegral sW / 2    else cam
    activeFballs  = if respawning then []                    else fb4
    activeBAnims  = if respawning then []                    else brickAnims'

    gsTemp = gs { gMario     = m7
                , gTiles     = activeTiles
                , gEnem      = activeEnem
                , gPups      = activePups
                , gCoins     = activeCoins
                , gScore     = sc5
                , gLives     = livesAfter
                , gCam       = activeCam
                , gPhase     = ph2
                , gFirebars  = fb_stepped
                , gFireballs = activeFballs
                , gTimer     = activeTimer
                , gCoinCount = newCoinCount
                , gBrickAnims = activeBAnims
                }

    gs' = case ph2 of
            LevelComplete -> advanceToNextLevel gsTemp
            _             -> gsTemp

advanceToNextLevel :: GS -> GS
advanceToNextLevel gs =
  let nextIdx = gLevelIdx gs + 1
  in if nextIdx < length (gLevels gs)
     then let nextLvl = gLevels gs !! nextIdx
          in gs { gMario     = initMarioFromLevel nextLvl
                , gTiles     = lTiles nextLvl
                , gEnem      = lEnemies nextLvl
                , gPups      = lPups nextLvl
                , gCoins     = lCoins nextLvl
                , gFirebars  = lFirebars nextLvl
                , gFireballs  = []
                , gCam        = fromIntegral sW / 2
                , gPhase      = Play
                , gLevelIdx   = nextIdx
                , gTimer      = 400
                , gBrickAnims = []
                }
     else gs { gPhase = Win }

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv (EventKey (Char d) Down _ _) gs
  | d >= '1' && d <= '8' = loadLevel (fromEnum d - fromEnum '1') gs
handleEv _ gs | gPhase gs /= Play = gs
handleEv ev gs = case ev of
  EventKey k Down _ _ -> gs { gMario = tryJump' k (joeCheck k (bufferKey k (gMario gs)))
                             , gKeys  = setK k True  (gKeys gs) }
  EventKey k Up   _ _ -> gs { gKeys  = setK k False (gKeys gs) }
  _ -> gs
  where
    -- Append letter to the buffer, keep last 3 chars only
    bufferKey (Char c) m | c `elem` ("joe" :: String) =
      let buf' = drop (max 0 (length (mJoeBuffer m) + 1 - 3)) (mJoeBuffer m ++ [c])
      in m { mJoeBuffer = buf' }
    bufferKey _ m = m

    -- Toggle joe mode if buffer == "joe" and Mario is Fire; reset buffer either way
    joeCheck (Char _) m
      | mJoeBuffer m == "joe" && mState m == Fire =
          m { mJoeMode = not (mJoeMode m), mJoeBuffer = "" }
      | mJoeBuffer m == "joe" =
          m { mJoeBuffer = "" }
    joeCheck _ m = m
    setK (Char 'a')            v k = k { kL   = v }
    setK (Char 'd')            v k = k { kR   = v }
    setK (Char 'w')            v k = k { kJ   = v }   -- ← ADD: W jumps
    setK (Char 's')            v k = k { kD   = v }   -- ← ADD: S crouches
    setK (SpecialKey KeyLeft)  v k = k { kL   = v }
    setK (SpecialKey KeyRight) v k = k { kR   = v }
    setK (SpecialKey KeySpace) v k = k { kJ   = v }
    setK (SpecialKey KeyUp)    v k = k { kJ   = v }
    setK (SpecialKey KeyDown)  v k = k { kD   = v }   -- ← ADD: arrow down also crouches
    setK (Char 'z')            v k = k { kRun = v }
    setK (Char 'x')            v k = k { kRun = v }
    setK _ _ k = k

    tryJump' (SpecialKey KeySpace) m = tryJump m
    tryJump' (SpecialKey KeyUp)    m = tryJump m
    tryJump' (Char 'w')            m = tryJump m       -- ← ADD: W also triggers jump
    tryJump' _ m = m    