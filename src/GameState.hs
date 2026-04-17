module GameState (initGS, step, handleEv) where

import Graphics.Gloss.Interface.Pure.Game
import Constants (sW, grav, ts)
import Types
import Physics (solid, physicsMario, mBB, hit, tBB, eBB)
import Mario (inputMario, tryJump, deathCheck)
import Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions)
import PowerUp (bumpBlocks, stepPup, grabPups, pickCoins)
import Level (allLevels, initMarioFromLevel)

initGS :: GS
initGS =
  let startLevel = allLevels !! 0
  in GS { gMario           = initMarioFromLevel startLevel
        , gTiles           = lTiles startLevel
        , gEnem            = lEnemies startLevel
        , gPups            = lPups startLevel
        , gCoins           = lCoins startLevel
        , gScore           = 0
        , gLives           = 3
        , gCam             = lStartX startLevel
        , gKeys            = KS False False False False
        , gPhase           = Play
        , gLevelIdx        = 0
        , gLevels          = allLevels
        , gFirebars        = lFirebars startLevel
        , gEnemyFireballs  = []
        , gMarioFireballs  = []
        , gTimer           = 400
        }

loadLevel :: Int -> GS -> GS
loadLevel idx gs
  | idx >= 0 && idx < length (gLevels gs) =
      let lvl = gLevels gs !! idx
      in gs { gMario           = initMarioFromLevel lvl
            , gTiles           = lTiles lvl
            , gEnem            = lEnemies lvl
            , gPups            = lPups lvl
            , gCoins           = lCoins lvl
            , gFirebars        = lFirebars lvl
            , gEnemyFireballs  = []
            , gMarioFireballs  = []
            , gCam             = lStartX lvl
            , gPhase           = Play
            , gLevelIdx        = idx
            }
  | otherwise = gs

findLevelIndex :: Int -> Int -> [Level] -> Int
findLevelIndex world num levels =
  case findIndex (\l -> lWorld l == world && lNumber l == num) levels of
    Just i  -> i
    Nothing -> 0
  where
    findIndex :: (Level -> Bool) -> [Level] -> Maybe Int
    findIndex p = go 0
      where
        go :: Int -> [Level] -> Maybe Int
        go _ [] = Nothing
        go n (x:xs) = if p x then Just n else go (n+1) xs

enterPipe :: GS -> GS
enterPipe gs =
  let currentLevel = gLevels gs !! gLevelIdx gs
      mario = gMario gs
      onPipe = filter (\t -> tType t == PipeTop && hit (mBB mario) (tBB t)) (gTiles gs)
  in case onPipe of
       (t:_) ->
         let key = (tCol t, tRow t)
         in case lookup key (lWarpMap currentLevel) of
              Just (world, lvl, x, y) ->
                let targetIdx = findLevelIndex world lvl (gLevels gs)
                    targetLvl = gLevels gs !! targetIdx
                in gs { gMario           = mario { mX = x, mY = y }
                      , gTiles           = lTiles targetLvl
                      , gEnem            = lEnemies targetLvl
                      , gPups            = lPups targetLvl
                      , gCoins           = lCoins targetLvl
                      , gFirebars        = lFirebars targetLvl
                      , gEnemyFireballs  = []
                      , gMarioFireballs  = []
                      , gLevelIdx        = targetIdx
                      , gPhase           = Play
                      }
              Nothing -> gs
       [] -> gs

stepFirebar :: Float -> Firebar -> Firebar
stepFirebar dt fb = fb { fbAngle = fbAngle fb + fbSpeed fb * dt }

stepFireball :: Float -> [Tile] -> Fireball -> Fireball
stepFireball dt sol fb
  | not (fbAlive fb) = fb
  | otherwise =
      let x' = fbx fb + fbvx fb * dt
          y' = fby fb + fbvy fb * dt
          vy' = fbvy fb + grav * dt
          hitWall = any (hit (x'+ts/2, y'+ts/2, ts*0.5, ts*0.5) . tBB) sol
          alive' = not hitWall && y' > -100
      in fb { fbx = x', fby = y', fbvy = vy', fbAlive = alive' }

fireballMarioCollisions :: Mario -> [Fireball] -> Int -> (Mario, [Fireball], Int)
fireballMarioCollisions m fbs sc = foldr go (m, [], sc) fbs
  where
    go fb (mario, acc, s)
      | not (fbAlive fb) = (mario, fb:acc, s)
      | hit (mBB mario) (fbx fb, fby fb, ts*0.5, ts*0.5) =
          if mInv mario > 0
          then (mario, fb:acc, s)
          else if mState mario == Big || mState mario == Fire
               then ( mario { mState = Small, mInv = 2.0 }
                    , fb { fbAlive = False } : acc, s )
               else ( mario { mState = MDead, mVY = 500, mVX = 0 }
                    , fb:acc, s )
      | otherwise = (mario, fb:acc, s)

marioFireballEnemyCollisions :: [Enemy] -> [Fireball] -> Int -> ([Enemy], [Fireball], Int)
marioFireballEnemyCollisions es fbs sc = foldr go (es, [], sc) fbs
  where
    go fb (enemies, accFbs, s)
      | not (fbAlive fb) = (enemies, fb:accFbs, s)
      | otherwise =
          let (enemies', killed, sAdd) = foldr (checkHit fb) ([], False, 0) enemies
          in if killed
             then (enemies', fb { fbAlive = False } : accFbs, s + sAdd)
             else (enemies, fb:accFbs, s)

    checkHit fb e (es, killed, sAdd) =
      if hit (fbx fb, fby fb, ts*0.5, ts*0.5) (eBB e)
         && case eState e of EDead _ -> False; _ -> True
      then (e { eState = EDead 0.5 } : es, True, sAdd + 200)
      else (e:es, killed, sAdd)

spawnMarioFireball :: Mario -> Fireball
spawnMarioFireball m =
  Fireball { fbx = mX m + fromIntegral (mFace m) * 20
           , fby = mY m + 10
           , fbvx = fromIntegral (mFace m) * 400
           , fbvy = 100
           , fbAlive = True
           }

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

    cam = max (gCam gs) (mX m4)

    (es1, newFireballs) = unzip $ map (stepEnemy dt sol) (gEnem gs)
    es1' = handleShellEnemyCollisions es1
    es2 = filter (\e -> case eState e of
                          EDead t -> t > 0
                          _       -> True) es1'

    (m5, es3, sc1) = collideEnemies m4 es2 (gScore gs) (kJ ks)
    (cs, sc2)      = pickCoins (mBB m5) (gCoins gs) sc1
    (ts2, pu1, sc3)= bumpBlocks m5 (mVY m0) (gTiles gs) (gPups gs) sc2
    pu2            = map (stepPup dt (filter (solid . tType) ts2)) pu1
    (m6, pu3, sc4, livesDelta) = grabPups m5 pu2 sc3

    onLava = any (\t -> tRow t == (-2) && hit (mBB m6) (tBB t)) (gTiles gs)
    (m7, ph) = if onLava
                 then (m6 { mState = MDead, mVY = 500, mVX = 0 }, Play)
                 else deathCheck m6 (gLives gs + livesDelta)

    m8 = case mState m7 of
           Star t -> let t' = t - dt in if t' <= 0 then m7 { mState = Big } else m7 { mState = Star t' }
           _      -> m7

    currentLevel = gLevels gs !! gLevelIdx gs
    endX = lEndX currentLevel

    touchedAxe = any (\t -> tType t == Axe && hit (mBB m8) (tBB t)) (gTiles gs)
    ph2 | touchedAxe = Win
        | ph == Play && mX m8 >= endX = LevelComplete
        | otherwise = ph

    fb1 = map (stepFirebar dt) (gFirebars gs)
    allEnemyFbs = gEnemyFireballs gs ++ concat newFireballs
    enemyFbsStepped = map (stepFireball dt sol) allEnemyFbs
    enemyFbsAlive = filter fbAlive enemyFbsStepped
    (m9, enemyFbsFinal, sc5) = fireballMarioCollisions m8 enemyFbsAlive sc4

    -- Mario fireballs
    marioFbsStepped = map (stepFireball dt sol) (gMarioFireballs gs)
    marioFbsAlive = filter fbAlive marioFbsStepped
    (es4, marioFbsFinal, sc6) = marioFireballEnemyCollisions es3 marioFbsAlive sc5

    newTimer = max 0 (gTimer gs - dt)

    gsTemp = gs { gMario           = m9
                , gTiles           = ts2
                , gEnem            = es4
                , gPups            = pu3
                , gCoins           = cs
                , gScore           = sc6
                , gLives           = gLives gs + livesDelta
                , gCam             = cam
                , gPhase           = ph2
                , gFirebars        = fb1
                , gEnemyFireballs  = enemyFbsFinal
                , gMarioFireballs  = marioFbsFinal
                , gTimer           = newTimer }

    gs' = case ph2 of
            LevelComplete -> advanceToNextLevel gsTemp
            _             -> gsTemp

advanceToNextLevel :: GS -> GS
advanceToNextLevel gs =
  let nextIdx = gLevelIdx gs + 1
  in if nextIdx < length (gLevels gs)
     then let nextLvl = gLevels gs !! nextIdx
          in gs { gMario           = initMarioFromLevel nextLvl
                , gTiles           = lTiles nextLvl
                , gEnem            = lEnemies nextLvl
                , gPups            = lPups nextLvl
                , gCoins           = lCoins nextLvl
                , gFirebars        = lFirebars nextLvl
                , gEnemyFireballs  = []
                , gMarioFireballs  = []
                , gCam             = lStartX nextLvl
                , gPhase           = Play
                , gLevelIdx        = nextIdx
                , gTimer           = 400
                }
     else gs { gPhase = Win }

handleEv :: Event -> GS -> GS
handleEv (EventKey (Char 'r') Down _ _) _ = initGS
handleEv (EventKey (Char d) Down _ _) gs
  | d >= '1' && d <= '4' = loadLevel (fromEnum d - fromEnum '1') gs
handleEv (EventKey (Char 'c') Down _ _) gs
  | gPhase gs == Play && mState (gMario gs) == Fire =
      let mario = gMario gs
          newFb = spawnMarioFireball mario
      in gs { gMarioFireballs = newFb : gMarioFireballs gs }
handleEv (EventKey (SpecialKey KeyDown) Down _ _) gs
  | gPhase gs == Play = enterPipe gs
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