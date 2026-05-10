module Enemy (stepEnemy, collideEnemies, handleShellEnemyCollisions, handleEnemyEnemyCollisions) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB, eBB, solid)

stepEnemy :: Float -> [Tile] -> Mario -> Enemy -> Enemy
stepEnemy dt sol mario e = case eState e of
  EAlive
    | eType e == CheepCheep -> stepCheepCheep dt sol mario e
    | eType e == GreenCheep -> stepCheepCheep dt sol mario e
    | eType e == Blooper    -> stepBlooper dt sol mario e
    | otherwise             -> stepAlive dt sol mario e

  EBowser _ _ _ _ -> stepAlive dt sol mario e

  EDead timer ->
    let t' = timer - dt
    in if t' <= 0 then e { eState = EDead 0 }
                  else e { eState = EDead t' }

  EShell timer moving ->
    let t' = timer - dt
        e' = if moving then stepShellMoving dt sol e else stepShellStationary dt sol e
    in if t' <= 0 && not moving
       then e' { eState = EAlive, eVX = -70, eVY = 0 }
       else e' { eState = EShell t' moving }

  EPiranha timer up ->
    stepPiranha dt mario e timer up

stepAlive :: Float -> [Tile] -> Mario -> Enemy -> Enemy
stepAlive dt sol mario e
  | eType e == Piranha = e
  | eType e == Bowser  = stepBowser dt sol mario e
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

-- | Piranha plant behavior.
--   eVY stores the fixed base Y so the plant does not drift upward forever.
--   The plant stays hidden if Mario is close to the pipe.
stepPiranha :: Float -> Mario -> Enemy -> Float -> Bool -> Enemy
stepPiranha dt mario e timer up =
  e { eY = y'
    , eVY = baseY
    , eState = EPiranha timer'' up'
    }
  where
    baseY =
      if eVY e == 0
        then eY e
        else eVY e

    pipeCenterX = eX e + ts
    marioNearPipe = abs (mX mario - pipeCenterX) < ts * 2.25

    timer' = timer - dt

    (up', timer'')
      | not up && marioNearPipe =
          (False, 1.2)

      | timer' <= 0 && up =
          (False, 1.4)

      | timer' <= 0 && not up =
          (True, 1.8)

      | otherwise =
          (up, timer')

    targetY =
      if up'
        then baseY + ts
        else baseY - 72   -- descend full sprite height (48x72) below baseY to hide completely

    speed = ts * 1.5
    dy = targetY - eY e
    maxStep = speed * dt

    y'
      | abs dy <= maxStep = targetY
      | otherwise         = eY e + signum dy * maxStep

-- | Cheep-cheep: simple underwater fish movement.
--   Swims horizontally and bounces off walls/solid blocks.
stepCheepCheep :: Float -> [Tile] -> Mario -> Enemy -> Enemy
stepCheepCheep dt sol _ e = e { eX = x', eY = y', eVX = vx', eVY = vy' }
  where
    vx0 = if abs (eVX e) < 1 then -90 else eVX e
    vy0 = if abs (eVY e) < 1 then  20 else eVY e

    x0 = eX e + vx0 * dt
    y0 = eY e + vy0 * dt

    bbAt x y = (x + ts/2, y + ts/2, ts*0.75, ts*0.75)

    hitX = any (hit (bbAt x0 (eY e)) . tBB) sol
    hitY = any (hit (bbAt (eX e) y0) . tBB) sol
        || y0 < ts * 2
        || y0 > ts * 9

    x'  = if hitX then eX e else x0
    y'  = if hitY then eY e else y0
    vx' = if hitX then -vx0 else vx0
    vy' = if hitY then -vy0 else vy0


-- | Blooper: slow underwater enemy that drifts toward Mario.
--   This is not exact NES behavior yet, but it gives the level the right feel.
stepBlooper :: Float -> [Tile] -> Mario -> Enemy -> Enemy
stepBlooper dt sol mario e = e { eX = x', eY = y', eVX = vx', eVY = vy' }
  where
    dirX :: Float
    dirX = if mX mario < eX e then -1 else 1

    dirY :: Float
    dirY = if mY mario > eY e then 1 else -1

    pulse = sin (mAnim mario * 2.5 + eX e / 50)

    desiredVX = 28 * dirX
    desiredVY = 40 * dirY + 18 * pulse

    vx0 = eVX e * 0.94 + desiredVX * 0.06
    vy0 = eVY e * 0.92 + desiredVY * 0.08

    x0 = eX e + vx0 * dt
    y0 = eY e + vy0 * dt

    bbAt x y = (x + ts/2, y + ts/2, ts*0.75, ts*0.9)

    hitX = any (hit (bbAt x0 (eY e)) . tBB) sol
    hitY = any (hit (bbAt (eX e) y0) . tBB) sol
        || y0 < ts * 2
        || y0 > ts * 9

    x'  = if hitX then eX e else x0
    y'  = if hitY then eY e else y0
    vx' = if hitX then -vx0 else vx0
    vy' = if hitY then -vy0 else vy0

-- | Bowser behavior:
--   * Stands idle until Mario gets within ~8 tiles
--   * Always walks toward Mario once triggered
--   * Jumps frequently on a timer
--   * Fire timer counts down to 0 as a signal for GameState to spawn a fireball
--   * Hit points stored in EBowser state; defeated by 5 fireballs or the axe
stepBowser :: Float -> [Tile] -> Mario -> Enemy -> Enemy
stepBowser dt sol mario e =
  e { eX    = ex'
    , eVX   = vx'
    , eY    = ey'
    , eVY   = vy'
    , eState = newState
    }
  where
    (ft, jt, it, hp) = case eState e of
      EBowser f j i h -> (f, j, i, h)
      _               -> (3.0, 2.5, 1.0, 5)

    -- Proximity trigger: once Mario is within 8 tiles, Bowser activates forever.
    marioClose = abs (mX mario - eX e) < ts * 8
    it' | it > 0 && marioClose = 0
        | it > 0               = it
        | otherwise            = 0

    idle = it' > 0

    -- Fire timer: counts down to 0 as spawn signal; GameState resets it to 3.0
    ft' | idle         = ft
        | ft - dt <= 0 = 0
        | otherwise    = ft - dt

    -- Jump timer: auto-resets every 2.5s for frequent hops
    jt' | idle         = jt
        | jt - dt <= 0 = 2.5
        | otherwise    = jt - dt

    newState = EBowser ft' jt' it' hp

    -- Always walk toward Mario
    dir :: Float
    dir = if mX mario < eX e then -1 else 1

    vx' | idle      = 0
        | otherwise = 90 * dir

    ex0     = eX e + vx' * dt
    wallHit = any (hit (ex0 + ts, eY e + ts, ts * 1.5, ts * 1.5) . tBB) sol
    ex'     = if wallHit then eX e else ex0

    -- Gravity and landing
    vy0 = eVY e + grav * dt
    ey0 = eY e + vy0 * dt

    landTiles =
      filter
        (\t ->
          let tTop = fromIntegral (tRow t) * ts + ts
          in eY e >= tTop
             && ey0 < tTop
             && abs (ex' + ts - (fromIntegral (tCol t) * ts + ts / 2)) < ts * 1.5
        )
        sol

    onG   = not (null landTiles)
    snapY = maximum (map (\t -> fromIntegral (tRow t) * ts + ts) landTiles)

    jumpNow = not idle && jt - dt <= 0 && onG

    (ey', vy')
      | onG && not jumpNow = (snapY, 0)
      | jumpNow            = (eY e, 500)
      | otherwise          = (ey0, vy0)

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

-- | Bounce alive enemies off each other by reversing the velocity of whichever
--   enemy is moving toward the other. Only one needs to turn — the other will
--   naturally separate on the next frame.
handleEnemyEnemyCollisions :: [Enemy] -> [Enemy]
handleEnemyEnemyCollisions es = map bounce es
  where
    bounce e
      | not (isWalking e) = e
      | any (movingToward e) es = e { eVX = -(eVX e) }
      | otherwise = e

    -- True if 'other' is a walking enemy overlapping 'e' and 'e' is heading toward it
    movingToward e other =
      not (eX e == eX other && eY e == eY other)
      && isWalking other
      && hit (eBB e) (eBB other)
      && movingCloser e other

    -- Is e moving in the direction of other?
    movingCloser e other
      | eX e < eX other = eVX e > 0   -- other is to the right, e moving right
      | otherwise       = eVX e < 0   -- other is to the left,  e moving left

    isWalking e = case eState e of
      EAlive          -> True
      EBowser _ _ _ _ -> True
      _               -> False

marioHalfHeight :: Mario -> Float
marioHalfHeight m = if mState m == Big || mState m == Fire then ts else ts/2

collideEnemies :: Mario -> [Enemy] -> Int -> Bool -> (Mario, [Enemy], Int)
collideEnemies m es sc jumpHeld = foldr go (m, [], sc) es
  where
    go e (mario, acc, s)
      | mState mario == MDead       = (mario, e:acc, s)
      | shouldIgnore e              = (mario, e:acc, s)
      | not (hit (mBB mario) (eBB e)) = (mario, e:acc, s)
      | otherwise                   = handleCollision mario e acc s

    shouldIgnore e = case (eType e, eState e) of
      (_, EDead _) -> True

      -- Hidden Piranhas should not hurt Mario.
      -- If the plant is still visibly retracting, it can still hurt him.
      (Piranha, EPiranha _ False) ->
        eY e <= eVY e + 2

      _ -> False
 
    handleCollision mario e acc s
      -- Stomp: Mario must be falling (mVY < 0) and above the enemy's centre.
      -- Bowser is immune to stomps — only the axe kills him.
      | mY mario > eY e + ts*0.55 && mVY mario < 0 =
          let bounce      = bounceVel jumpHeld
              marioBounce = mario { mY  = eY e + ts*0.55 + marioHalfHeight mario
                                  , mVY = bounce }
          in case eType e of
            Bowser  -> hurtMario mario e acc s   -- stomp does nothing to Bowser
            Piranha -> hurtMario mario e acc s   -- stomp does nothing to Piranha
            Goomba  -> ( marioBounce
                       , e { eState = EDead 0.5 } : acc, s + 100 )
            Koopa   -> case eState e of
              EAlive ->
                ( marioBounce { mInv = 0.3 }
                , e { eState = EShell 5.0 False } : acc, s + 100 )
              EShell _ False ->
                ( marioBounce { mInv = 0.05 }, e:acc, s )
              EShell _ True ->
                ( marioBounce { mInv = 0.3 }
                , e { eState = EShell 5.0 False } : acc, s + 100 )
              _ -> (mario, e:acc, s)
            CheepCheep -> hurtMario mario e acc s
            GreenCheep -> hurtMario mario e acc s
            Blooper    -> hurtMario mario e acc s

      -- Kick stationary shell: Mario must be moving toward it (not just touching)
      | eType e == Koopa && isStationaryShell e =
          if mInv mario <= 0
            then let dir       = if mX mario < eX e then 1 else -1
                     kickSpeed = 600 * fromIntegral dir
                     shellX    = eX e + fromIntegral dir * 40
                     kicked    = e { eState = EShell 5.0 True
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
      EAlive            -> True
      EBowser _ _ _ _   -> True
      EShell _ True     -> True
      EPiranha _ _      -> eY e > eVY e + 2
      _                 -> False

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