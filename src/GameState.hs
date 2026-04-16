module GameState (initGS, step, handleEv) where

import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, grav, ts)
import Types
import Physics (solid, physicsMario, mBB, hit, tBB)
import Mario (inputMario, tryJump, deathCheck)
import Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions)
import PowerUp (bumpBlocks, stepPup, grabPups, pickCoins)
import Level (allLevels, initMarioFromLevel)

initGS :: GS
initGS =
  let startLevel = allLevels !! 0
  in GS { gMario    = initMarioFromLevel startLevel
        , gTiles    = lTiles startLevel
        , gEnem     = lEnemies startLevel
        , gPups     = lPups startLevel
        , gCoins    = lCoins startLevel
        , gScore    = 0
        , gLives    = 3
        , gCam      = 0
        , gKeys     = KS False False False False
        , gPhase    = Play
        , gLevelIdx = 0
        , gLevels   = allLevels
        , gFirebars = lFirebars startLevel
        }

loadLevel :: Int -> GS -> GS
loadLevel idx gs
  | idx >= 0 && idx < length (gLevels gs) =
      let lvl = gLevels gs !! idx
      in gs { gMario    = initMarioFromLevel lvl
            , gTiles    = lTiles lvl
            , gEnem     = lEnemies lvl
            , gPups     = lPups lvl
            , gCoins    = lCoins lvl
            , gFirebars = lFirebars lvl
            , gCam      = 0
            , gPhase    = Play
            , gLevelIdx = idx
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

    m0  = inputMario ks (gMario gs)
    m1  = if mState m0 == MDead
            then m0 { mVY = max (-900) (mVY m0 + grav * dt)
                    , mY  = mY m0 + mVY m0 * dt }
            else physicsMario dt sol m0
    m4  = m1 { mAnim = mAnim m1 + dt
              , mInv = max 0 (mInv m1 - dt) }

    cam = max (gCam gs) (mX m4 - fromIntegral sW * 0.35)

    es1 = map (stepEnemy dt sol) (gEnem gs)
    es1' = handleShellEnemyCollisions es1
    es2 = filter (\e -> case eState e of
                          EDead t -> t > 0
                          _       -> True) es1'

    (m5, es3, sc1) = collideEnemies m4 es2 (gScore gs) (kJ ks)
    (cs, sc2)      = pickCoins (mBB m5) (gCoins gs) sc1
    (ts2, pu1, sc3)= bumpBlocks m5 (mVY m0) (gTiles gs) (gPups gs) sc2
    pu2            = map (stepPup dt (filter (solid . tType) ts2)) pu1
    (m6, pu3, sc4) = grabPups m5 pu2 sc3

    onLava = any (\t -> tRow t == (-2) && hit (mBB m6) (tBB t)) (gTiles gs)
    (m7, ph) = if onLava
                 then (m6 { mState = MDead, mVY = 500, mVX = 0 }, Play)
                 else deathCheck m6 (gLives gs)

    currentLevel = gLevels gs !! gLevelIdx gs
    endX = lEndX currentLevel

    touchedAxe = any (\t -> tType t == Axe && hit (mBB m7) (tBB t)) (gTiles gs)
    ph2 | touchedAxe = Win
        | ph == Play && mX m7 >= endX = LevelComplete
        | otherwise = ph

    fb1 = map (stepFirebar dt) (gFirebars gs)

    gsTemp = gs { gMario = m7
                , gTiles = ts2
                , gEnem  = es3
                , gPups  = pu3
                , gCoins = cs
                , gScore = sc4
                , gLives = if ph /= Play then max 0 (gLives gs - 1) else gLives gs
                , gCam   = cam
                , gPhase = ph2
                , gFirebars = fb1 }

    gs' = case ph2 of
            LevelComplete -> advanceToNextLevel gsTemp
            _             -> gsTemp

advanceToNextLevel :: GS -> GS
advanceToNextLevel gs =
  let nextIdx = gLevelIdx gs + 1
  in if nextIdx < length (gLevels gs)
     then let nextLvl = gLevels gs !! nextIdx
          in gs { gMario    = initMarioFromLevel nextLvl
                , gTiles    = lTiles nextLvl
                , gEnem     = lEnemies nextLvl
                , gPups     = lPups nextLvl
                , gCoins    = lCoins nextLvl
                , gFirebars = lFirebars nextLvl
                , gCam      = 0
                , gPhase    = Play
                , gLevelIdx = nextIdx
                }
     else gs { gPhase = Win }

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv (EventKey (Char d) Down _ _) gs
  | d >= '1' && d <= '4' = loadLevel (fromEnum d - fromEnum '1') gs
handleEv _ gs | gPhase gs /= Play = gs
handleEv ev gs = case ev of
  EventKey k Down _ _ -> gs { gMario = tryJump' k (gMario gs)
                            , gKeys  = setK k True  (gKeys gs) }
  EventKey k Up   _ _  -> gs { gKeys  = setK k False (gKeys gs) }
  _ -> gs
  where
    setK (Char 'a')             v k = k { kL   = v }
    setK (Char 'd')             v k = k { kR   = v }
    setK (SpecialKey KeyLeft)   v k = k { kL   = v }
    setK (SpecialKey KeyRight)  v k = k { kR   = v }
    setK (SpecialKey KeySpace)  v k = k { kJ   = v }
    setK (SpecialKey KeyUp)     v k = k { kJ   = v }
    setK (Char 'z')             v k = k { kRun = v }
    setK (Char 'x')             v k = k { kRun = v }
    setK _ _ k = k

    tryJump' (SpecialKey KeySpace) m = tryJump m
    tryJump' (SpecialKey KeyUp)    m = tryJump m
    tryJump' _ m = m