module Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB, eBB, solid)
import Debug.Trace (trace)

-- | Update an enemy, returning the updated enemy and a list of spawned fireballs.
stepEnemy :: Float -> [Tile] -> Enemy -> (Enemy, [Fireball])
stepEnemy dt sol e = case eState e of
  EAlive      -> (stepAlive dt sol e, [])
  EDead timer -> let t' = timer - dt
                     e' = if t' <= 0 then e { eState = EDead 0 } else e { eState = EDead t' }
                 in (e', [])
  EShell timer moving ->
    let t' = timer - dt
        e' = if moving then stepShellMoving dt sol e else stepShellStationary dt sol e
        e'' = if t' <= 0
              then trace "Shell expired, reverting to alive" e' { eState = EAlive, eVX = -70, eVY = 0 }
              else e' { eState = EShell t' moving }
    in (e'', [])
  EPiranha timer up ->
    let t' = timer - dt
        (newUp, newTimer) = if t' <= 0
                            then (not up, if up then 2.0 else 1.5)
                            else (up, t')
        baseY = fromIntegral (floor (eY e / ts)) * ts
        targetY = if newUp then baseY + ts else baseY
        e' = e { eState = EPiranha newTimer newUp, eY = targetY }
    in (e', [])
  EBowser shootTimer dir ->
    let t' = shootTimer - dt
        (newTimer, mbFireball) = if t' <= 0
                                 then (2.0, Just (spawnFireball e dir))
                                 else (t', Nothing)
        ex0 = eX e + dir * 50 * dt
        wallX = any (hit (ex0+ts/2, eY e+ts/2, ts*1.2, ts*1.2) . tBB) sol
        newDir = if wallX then -dir else dir
        ex' = if wallX then eX e else max 0 ex0
        e' = e { eX = ex', eVX = newDir * 50, eState = EBowser newTimer newDir }
    in (e', maybe [] (:[]) mbFireball)

spawnFireball :: Enemy -> Float -> Fireball
spawnFireball e dir =
  Fireball { fbx = eX e + ts/2 + dir * 20
           , fby = eY e + ts*0.8
           , fbvx = dir * 200
           , fbvy = 150
           , fbAlive = True
           }

stepAlive :: Float -> [Tile] -> Enemy -> Enemy
stepAlive dt sol e
  | eType e == Piranha = e
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
marioHalfHeight m = if mState m == Big then ts else ts/2

collideEnemies :: Mario -> [Enemy] -> Int -> Bool -> (Mario, [Enemy], Int)
collideEnemies m es sc jumpHeld = foldr go (m, [], sc) es
  where
    go e (mario, acc, s)
      | shouldIgnore e = (mario, e:acc, s)
      | not (hit (mBB mario) (eBB e)) = (mario, e:acc, s)
      | otherwise = handleCollision mario e acc s

    shouldIgnore e = case eState e of
      EDead _ -> True
      _       -> False

    handleCollision mario e acc s
      -- Stomp: Mario falling onto enemy
      | mY mario > eY e + ts*0.55 && mVY mario < 50 =
          let bounce = bounceVel jumpHeld
              marioBounce = mario { mY = eY e + ts*0.55 + marioHalfHeight mario, mVY = bounce }
          in case eType e of
            Goomba -> ( marioBounce { mInv = 0.3 }
                      , e { eState = EDead 0.5 } : acc, s + 100 )
            Koopa -> case eState e of
              EAlive ->
                trace "Stomp: Koopa -> stationary shell" $
                ( marioBounce { mInv = 0.5 }
                , e { eState = EShell 15.0 False } : acc, s + 100 )
              EShell _ False ->
                ( marioBounce { mInv = 0.05 }, e:acc, s )
              EShell _ True ->
                ( marioBounce { mInv = 0.5 }
                , e { eState = EShell 15.0 False } : acc, s + 100 )
              _ -> (mario, e:acc, s)
            Piranha -> hurtMario mario e acc s
            Bowser ->
              ( marioBounce { mInv = 0.5 }
              , e { eState = EBowser 1.5 (if eVX e < 0 then -1 else 1) } : acc, s + 5000 )

      -- Kick stationary shell
      | eType e == Koopa && isStationaryShell e =
          trace ("!!! Collision with stationary shell! mInv=" ++ show (mInv mario) ++
                 " mX=" ++ show (mX mario) ++ " eX=" ++ show (eX e)) $
          if mInv mario <= 0.05
            then let dir = if mX mario < eX e then 1 else -1
                     kickSpeed = 600 * fromIntegral dir
                     shellX = eX e + fromIntegral dir * 40
                     kicked = e { eState = EShell 15.0 True, eX = shellX, eVX = kickSpeed, eVY = 150 }
                     mario' = mario { mX = mX mario + fromIntegral (-dir) * 40
                                    , mVY = 180
                                    , mGround = False
                                    , mInv = 0.8 }
                 in trace ("***** KICK! dir=" ++ show dir ++ " *****") (mario', kicked:acc, s + 200)
            else trace ("Kick blocked by invincibility: " ++ show (mInv mario)) (mario, e:acc, s)

      | mInv mario > 0 = (mario, e:acc, s)

      | isDangerous e = hurtMario mario e acc s

      | otherwise = (mario, e:acc, s)

    isStationaryShell e = case eState e of EShell _ False -> True; _ -> False
    isDangerous e = case eState e of
                      EAlive -> True
                      EShell _ True -> True
                      EBowser _ _ -> True
                      _ -> False

    hurtMario mario e acc s
      | mState mario == Big =
          let knockbackDir = if mX mario < eX e then -1 else 1
              knockbackX = 240 * fromIntegral knockbackDir
          in ( mario { mState = Small, mInv = 2.0, mVX = knockbackX }
             , e:acc, s )
      | otherwise =
          ( mario { mState = MDead, mVY = 500, mVX = 0 }, e:acc, s )

    bounceVel True  = 520
    bounceVel False = 360