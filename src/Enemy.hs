module Enemy (stepEnemy, collideEnemies) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB, eBB, solid)

stepEnemy :: Float -> [Tile] -> Enemy -> Enemy
stepEnemy dt sol e = case eState e of
  EAlive      -> stepAlive dt sol e
  EDead timer -> let t' = timer - dt
                 in if t' <= 0
                    then e { eState = EDead 0 }
                    else e { eState = EDead t' }
  EShell timer moving ->
    let t' = timer - dt
        e' = if moving then stepShellMoving dt sol e else stepShellStationary dt sol e
    in if t' <= 0
       then e' { eState = EDead 0 }
       else e' { eState = EShell t' moving }
  EPiranha timer up ->
    let t' = timer - dt
        (newUp, newTimer) = if t' <= 0
                            then (not up, if up then 2.0 else 1.5)
                            else (up, t')
        -- Target Y: when up, sit at pipe top (row+1); when down, hide at pipe interior (row)
        baseY = fromIntegral (floor (eY e / ts)) * ts
        targetY = if newUp then baseY + ts else baseY
        -- Move smoothly toward target (but for simplicity, snap)
        newY = targetY
    in e { eState = EPiranha newTimer newUp, eY = newY }

stepAlive :: Float -> [Tile] -> Enemy -> Enemy
stepAlive dt sol e
  | eType e == Piranha = e  -- Piranha handled by EPiranha state
  | otherwise = e { eX = ex', eVX = vx', eY = ey', eVY = vy' }
  where
    ex0   = eX e + eVX e * dt
    wallX = any (hit (ex0+ts/2, eY e+ts/2, ts*0.7, ts*0.7) . tBB) sol
    vx1   = if wallX then -eVX e else eVX e
    ex1   = if wallX then eX e else max 0 ex0

    aheadX = ex1 + (if vx1 > 0 then ts else -ts)
    probe  = (aheadX+ts/2, eY e-ts/4, ts*0.5, ts*0.5)
    edge   = not (any (hit probe . tBB) sol)
    vx2    = if edge then -vx1 else vx1
    ex'    = ex1
    vx'    = vx2

    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    onG   = any (hit (ex'+ts/2, ey0+ts/2, ts*0.5, ts*0.5) . tBB) sol
    (ey', vy') = if onG then (eY e, 0) else (ey0, vy0)

stepShellStationary :: Float -> [Tile] -> Enemy -> Enemy
stepShellStationary dt sol e = e { eY = ey', eVY = vy' }
  where
    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    onG   = any (hit (eX e+ts/2, ey0+ts/2, ts*0.7, ts*0.7) . tBB) sol
    (ey', vy') = if onG then (eY e, 0) else (ey0, vy0)

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
      | mInv mario > 0 = (mario, e:acc, s)

      -- Mario lands on top of enemy
      | mY mario > eY e + ts*0.55 && mVY mario < 50 =
          case eType e of
            Goomba ->
              ( mario { mVY = bounceVel jumpHeld }
              , e { eState = EDead 0.5 } : acc
              , s + 100 )
            Koopa ->
              case eState e of
                EAlive ->
                  ( mario { mVY = bounceVel jumpHeld }
                  , e { eState = EShell 8.0 False } : acc
                  , s + 100 )
                EShell _ False ->
                  -- Stomping stationary shell does nothing (no score, no state change)
                  ( mario { mVY = bounceVel jumpHeld }, e:acc, s )
                EShell _ True ->
                  -- Stomping moving shell stops it
                  ( mario { mVY = bounceVel jumpHeld }
                  , e { eState = EShell 8.0 False } : acc
                  , s + 100 )
                _ -> (mario, e:acc, s)
            Piranha ->
              -- Piranha hurts even when stomped? In SMB, you can't stomp Piranha safely.
              if mState mario == Big
                then ( mario { mState = Small, mInv = 2.0 }, e:acc, s )
                else ( mario { mState = MDead, mVY = 500, mVX = 0 }, e:acc, s )

      -- Mario kicks stationary Koopa shell from side
      | eType e == Koopa &&
        case eState e of EShell _ False -> True; _ -> False =
          let dir = if mX mario < eX e then 1 else -1
              kicked = e { eState = EShell 8.0 True, eVX = 400 * fromIntegral dir }
          in (mario, kicked:acc, s + 200)

      -- Mario hit by moving shell
      | eType e == Koopa &&
        case eState e of EShell _ True -> True; _ -> False =
          hurtMario mario e acc s

      -- Normal enemy contact (side/bottom)
      | otherwise = hurtMario mario e acc s

        hurtMario mario e acc s
      | mState mario == Big =
          let knockbackDir = if mX mario < eX e then -1 else 1
              knockbackX = 200 * fromIntegral knockbackDir
          in ( mario { mState = Small, mInv = 2.0, mVX = knockbackX }
             , e:acc, s )
      | otherwise =
          ( mario { mState = MDead, mVY = 500, mVX = 0 }, e:acc, s )

    bounceVel True  = 500
    bounceVel False = 340