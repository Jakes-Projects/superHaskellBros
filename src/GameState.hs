module GameState (initGS, step, handleEv) where

import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, grav, ts)
import Types
import Physics (solid, physicsMario, mBB)
import Mario (inputMario, tryJump, deathCheck)
import Enemy (stepEnemy, collideEnemies)
import PowerUp (bumpBlocks, stepPup, grabPups, pickCoins)
import Level (initMario, mkTiles, mkEnemies, mkCoins, initKS)

initGS :: GS
initGS = GS initMario mkTiles mkEnemies [] mkCoins 0 3 0 initKS Play

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
    es2 = filter (\e -> case eState e of
                          EDead t -> t > 0
                          _       -> True) es1

    -- Pass jump-held flag (kJ) to collideEnemies for variable bounce
    (m5, es3, sc1) = collideEnemies m4 es2 (gScore gs) (kJ ks)
    (cs, sc2)      = pickCoins (mBB m5) (gCoins gs) sc1
    (ts2, pu1, sc3)= bumpBlocks m5 (mVY m0) (gTiles gs) (gPups gs) sc2
    pu2            = map (stepPup dt (filter (solid . tType) ts2)) pu1
    (m6, pu3, sc4) = grabPups m5 pu2 sc3

    (m7, ph) = deathCheck m6 (gLives gs)
    ph2 | ph == Play && mX m6 >= 204*ts = Win
        | otherwise = ph

    gs' = gs { gMario = m7
             , gTiles = ts2
             , gEnem  = es3
             , gPups  = pu3
             , gCoins = cs
             , gScore = sc4
             , gLives = if ph /= Play then max 0 (gLives gs - 1) else gLives gs
             , gCam   = cam
             , gPhase = ph2 }

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv _ gs | gPhase gs /= Play = gs
handleEv ev gs = case ev of
  EventKey k Down _ _ -> gs { gMario = tryJump' k (gMario gs)
                             , gKeys  = setK k True  (gKeys gs) }
  EventKey k Up   _ _ -> gs { gKeys  = setK k False (gKeys gs) }
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