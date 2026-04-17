module Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB, eBB, solid)

stepEnemy :: Float -> [Tile] -> Enemy -> Enemy
stepEnemy dt sol e = case eState e of
  EAlive      -> stepAlive dt sol e
  EDead timer -> let t' = timer - dt
                 in if t' <= 0 then e { eState = EDead 0 }
                               else e { eState = EDead t' }
  EShell timer moving ->
    let t' = timer - dt
        e' = if moving then stepShellMoving dt sol e else stepShellStationary dt sol e
    in if t' <= 0
       then e' { eState = EAlive, eVX = -70, eVY = 0 }
       else e' { eState = EShell t' moving }
  EPiranha timer up ->
    let t' = timer - dt
        (newUp, newTimer) = if t' <= 0
                            then (not up, if up then 2.0 else 1.5)
                            else (up, t')
        baseY = fromIntegral (floor (eY e / ts)) * ts
        targetY = if newUp then baseY + ts else baseY
    in e { eState = EPiranha newTimer newUp, eY = targetY }

stepAlive :: Float -> [Tile] -> Enemy -> Enemy
stepAlive dt sol e
  | eType e == Piranha = e
  | eType e == Bowser  = stepBowser dt sol e
  | otherwise = e { eX = ex', eVX = vx', eY = ey', eVY = vy' }
  where
    ex0   = eX e + eVX e * dt
    wallX = any (hit (ex0+ts/2, eY e+ts/2, ts*0.7, ts*0.7) . tBB) sol
    vx1   = if wallX then -eVX e else eVX e
    ex1   = if wallX then eX e else max 0 ex0

    aheadX = ex1 + (if vx1 > 0 then ts else -ts)
    probe  = (aheadX+ts/2, eY e-ts/4, ts*0.5, ts*0.75)
    edge   = not (any (hit probe . tBB) sol)
    vx2    = if edge then -vx1 else vx1
    ex'    = ex1
    vx'    = vx2

    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    landTiles = filter (\t -> let tTop = fromIntegral (tRow t) * ts + ts
                              in eY e >= tTop && ey0 < tTop
                                 && abs (ex' + ts/2 - (fromIntegral (tCol t)*ts + ts/2)) < ts)
                       sol
    onG   = not (null landTiles)
    snapY = maximum (map (\t -> fromIntegral (tRow t) * ts + ts) landTiles)
    (ey', vy') = if onG then (snapY, 0) else (ey0, vy0)

-- | Bowser paces back and forth on the bridge, never turns at edges
--   (he'd fall off otherwise), and is not subject to gravity stacking.
stepBowser :: Float -> [Tile] -> Enemy -> Enemy
stepBowser dt sol e = e { eX = ex', eVX = vx', eY = ey', eVY = vy' }
  where
    ex0   = eX e + eVX e * dt
    wallX = any (hit (ex0 + ts, eY e + ts, ts*1.4, ts*1.4) . tBB) sol
    vx'   = if wallX then -eVX e else eVX e
    ex'   = if wallX then eX e   else ex0

    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    landTiles = filter (\t -> let tTop = fromIntegral (tRow t) * ts + ts
                              in eY e >= tTop && ey0 < tTop
                                 && abs (ex' + ts - (fromIntegral (tCol t)*ts + ts/2)) < ts*1.5)
                       sol
    onG   = not (null landTiles)
    snapY = if onG then maximum (map (\t -> fromIntegral (tRow t) * ts + ts) landTiles) else ey0
    (ey', vy') = if onG then (snapY, 0) else (ey0, vy0)

stepShellStationary :: Float -> [Tile] -> Enemy -> Enemy
stepShellStationary dt sol e = e { eY = ey', eVY = vy' }
  where
    vy0    = eVY e + grav * dt
    ey0    = eY e + vy0 * dt
    landTiles = filter (\t -> let tTop = fromIntegral (tRow t) * ts + ts
                              in eY e >= tTop && ey0 < tTop
                                 && abs (eX e + ts/2 - (fromIntegral (tCol t)*ts + ts/2)) < ts)
                       sol
    onG    = not (null landTiles)
    snapY  = if onG then maximum (map (\t -> fromIntegral (tRow t) * ts + ts) landTiles) else ey0
    (ey', vy') = if onG then (snapY, 0) else (ey0, vy0)

stepShellMoving :: Float -> [Tile] -> Enemy -> Enemy
stepShellMoving dt sol e = e { eX = ex', eVX = vx', eY = ey', eVY = vy' }
  where
    ex0   = eX e + eVX e * dt
    wallX = any (hit (ex0+ts/2, eY e+ts/2, ts*0.7, ts*0.7) . tBB) sol
    vx'   = if wallX then -eVX e else eVX e
    ex'   = if wallX then eX e else max 0 ex0

    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    onG   = any (hit (ex'+ts/2, ey0+ts/2, ts*0.7, ts*0.7) . tBB) sol
    (ey', vy') = if onG then (eY e, 0) else (ey0, vy0)

handleShellEnemyCollisions :: [Enemy] -> [Enemy]
handleShellEnemyCollisions es = map killIfCollided es
  where
    killIfCollided e = case eState e of
      EShell _ True -> e
      _ -> if any (doesKill e) es
           then e { eState = EDead 0.5 }
           else e

    doesKill victim shell = case eState shell of
      EShell _ True -> hit (eBB shell) (eBB victim)
      _ -> False

marioHalfHeight :: Mario -> Float
marioHalfHeight m = if mState m == Big || mState m == Fire then ts else ts/2

collideEnemies :: Mario -> [Enemy] -> Int -> Bool -> (Mario, [Enemy], Int)
collideEnemies m es sc jumpHeld = foldr go (m, [], sc) es
  where
    go e (mario, acc, s)
      | shouldIgnore e              = (mario, e:acc, s)
      | not (hit (mBB mario) (eBB e)) = (mario, e:acc, s)
      | otherwise                   = handleCollision mario e acc s

    shouldIgnore e = case eState e of
      EDead _ -> True
      _       -> False

    handleCollision mario e acc s
      -- Stomp: Mario falling onto enemy from above.
      -- Bowser is immune to stomps — only the axe kills him.
      | mY mario > eY e + ts*0.55 && mVY mario < 50 =
          let bounce     = bounceVel jumpHeld
              marioBounce = mario { mY  = eY e + ts*0.55 + marioHalfHeight mario
                                  , mVY = bounce }
          in case eType e of
            Bowser  -> hurtMario mario e acc s   -- stomp does nothing to Bowser
            Goomba  -> ( marioBounce { mInv = 0.3 }
                       , e { eState = EDead 0.5 } : acc, s + 100 )
            Koopa   -> case eState e of
              EAlive ->
                ( marioBounce { mInv = 0.5 }
                , e { eState = EShell 15.0 False } : acc, s + 100 )
              EShell _ False ->
                ( marioBounce { mInv = 0.05 }, e:acc, s )
              EShell _ True ->
                ( marioBounce { mInv = 0.5 }
                , e { eState = EShell 15.0 False } : acc, s + 100 )
              _ -> (mario, e:acc, s)
            Piranha -> hurtMario mario e acc s

      -- Kick stationary shell
      | eType e == Koopa && isStationaryShell e =
          if mInv mario <= 0.05
            then let dir       = if mX mario < eX e then 1 else -1
                     kickSpeed = 600 * fromIntegral dir
                     shellX    = eX e + fromIntegral dir * 40
                     kicked    = e { eState = EShell 15.0 True
                                   , eX = shellX, eVX = kickSpeed, eVY = 150 }
                     mario'    = mario { mX     = mX mario + fromIntegral (-dir) * 40
                                       , mVY    = 180
                                       , mGround = False
                                       , mInv   = 0.8 }
                 in (mario', kicked:acc, s + 200)
            else (mario, e:acc, s)

      | mInv mario > 0    = (mario, e:acc, s)
      | isDangerous e     = hurtMario mario e acc s
      | otherwise         = (mario, e:acc, s)

    isStationaryShell e = case eState e of EShell _ False -> True; _ -> False
    isDangerous e = case eState e of
      EAlive       -> True
      EShell _ True -> True
      _            -> False

    -- | Damage Mario by exactly one power level.
    --   Fire → Big → Small → MDead
    hurtMario mario e acc s
      | mState mario == Fire =
          let kd = knockbackDir mario e
          in ( mario { mState = Big, mInv = 2.0, mVX = 240 * fromIntegral kd }
             , e:acc, s )
      | mState mario == Big =
          let kd = knockbackDir mario e
          in ( mario { mState = Small, mInv = 2.0, mVX = 240 * fromIntegral kd }
             , e:acc, s )
      | otherwise =
          ( mario { mState = MDead, mVY = 500, mVX = 0 }, e:acc, s )

    knockbackDir mario e = if mX mario < eX e then -1 else 1 :: Int

    bounceVel True  = 520
    bounceVel False = 360