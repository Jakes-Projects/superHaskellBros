module Fireball (spawnFireball, stepFireball, fireballsVsEnemies) where

import Constants (ts, grav)
import Types
import Physics (hit, tBB, eBB, solid)

-- ─── Constants ───────────────────────────────────────────────────────────────

fireballSpeedX :: Float
fireballSpeedX = 340    -- horizontal speed (px/s)

fireBounceVY :: Float
fireBounceVY = 310      -- upward speed on each ground bounce

-- Fireballs fall faster than the bounce height can grow, giving ~3 bounces
-- before going off-screen, matching the original.
fireballGrav :: Float
fireballGrav = grav * 0.6   -- lighter than Mario (-1400 * 0.6 = -840)

fireballHalf :: Float
fireballHalf = ts * 0.2     -- half-size of the fireball hitbox

fireCooldown :: Float
fireCooldown = 0.4          -- minimum seconds between shots

maxFireballs :: Int
maxFireballs = 2            -- original game cap

-- ─── Spawn ───────────────────────────────────────────────────────────────────

-- | Create a new fireball from Mario's position, if all conditions are met.
--   Returns the (possibly unchanged) fireball list; the caller is responsible
--   for resetting mFireCool on Mario when a shot is actually fired.
spawnFireball :: Mario -> [Fireball] -> (Bool, [Fireball])
spawnFireball m fbs
  | mState m /= Fire                       = (False, fbs)
  | mFireCool m > 0                        = (False, fbs)
  | length (filter fiAlive fbs) >= maxFireballs = (False, fbs)
  | otherwise = (True, newFb : fbs)
  where
    dir   = fromIntegral (mFace m) :: Float
    -- Spawn at Mario's chest height, offset slightly in facing direction
    newFb = Fireball
              { fiX    = mX m + dir * ts * 0.5
              , fiY    = mY m + ts * 0.1
              , fiVX   = fireballSpeedX * dir
              , fiVY   = 0
              , fiAlive = True
              }

-- ─── Step ────────────────────────────────────────────────────────────────────

-- | Advance one fireball by dt seconds.
--   Bounces off solid tile tops; destroyed by side/ceiling contact or walls.
stepFireball :: Float -> [Tile] -> Fireball -> Fireball
stepFireball dt sol fb
  | not (fiAlive fb) = fb
  | otherwise        = fb { fiX = x', fiY = y', fiVY = vy', fiAlive = alive' }
  where
    solidTiles = filter (solid . tType) sol

    vy0 = fiVY fb + fireballGrav * dt
    x1  = fiX  fb + fiVX fb * dt
    y1  = fiY  fb + vy0 * dt

    -- Wall / ceiling collision: any solid tile the fireball's new bounding box
    -- overlaps horizontally while at its current height.
    wallHit = any (hit (x1, fiY fb, fireballHalf*2, fireballHalf*2) . tBB) solidTiles

    -- Ground bounce: fireball moving downward whose bottom edge crosses the
    -- top surface of a solid tile.
    landTiles = filter (\t ->
        let tTop = fromIntegral (tRow t) * ts + ts
            tcx  = fromIntegral (tCol t) * ts + ts / 2
        in  vy0 <= 0
         && (fiY fb - fireballHalf) >= tTop - 4
         && (y1  - fireballHalf) <  tTop
         && abs (x1 - tcx) < ts * 0.9
        ) solidTiles
    groundHit = not (null landTiles)
    snapY     = maximum
                  (map (\t -> fromIntegral (tRow t) * ts + ts + fireballHalf)
                       landTiles)

    -- Off-screen (fell into a pit or flew too far back)
    offScreen = fiY fb < -400

    x'     = if wallHit  then fiX fb else x1
    y'     | groundHit  = snapY
            | otherwise = y1
    vy'    | wallHit    = vy0              -- keep falling after a wall kill
            | groundHit = fireBounceVY
            | otherwise = vy0
    alive' = not wallHit && not offScreen

-- ─── Enemy collision ─────────────────────────────────────────────────────────

-- | Check every active fireball against every vulnerable enemy.
--   Returns (updated fireballs, updated enemies, score delta).
--   Bowser is immune — only the axe kills him.
fireballsVsEnemies :: [Fireball] -> [Enemy] -> Int
                   -> ([Fireball], [Enemy], Int)
fireballsVsEnemies fbs es sc = (fbs', es', sc')
  where
    (fbs', es', sc') = foldr go ([], es, sc) fbs

    go fb (accFbs, accEs, accSc)
      | not (fiAlive fb) = (fb : accFbs, accEs, accSc)
      | otherwise =
          let (didHit, accEs', pts) = checkVsEnemies fb accEs
          in ( fb { fiAlive = not didHit } : accFbs
             , accEs'
             , accSc + pts )

checkVsEnemies :: Fireball -> [Enemy] -> (Bool, [Enemy], Int)
checkVsEnemies fb = foldr go (False, [], 0)
  where
    fiBB = (fiX fb, fiY fb, fireballHalf*2, fireballHalf*2)

    go e (didHit, accEs, pts)
      | isImmune e              = (didHit, e : accEs, pts)
      | not (hit fiBB (eBB e))  = (didHit, e : accEs, pts)
      | otherwise               = (True,   killEnemy e : accEs, pts + 200)

    isImmune e = case (eType e, eState e) of
      (Bowser, _)          -> True   -- only the axe kills Bowser
      (_, EDead _)         -> True   -- already dead
      (Piranha, EPiranha _ False) -> True  -- retracted Piranha can't be hit
      _                    -> False

    killEnemy e = e { eState = EDead 0.5 }