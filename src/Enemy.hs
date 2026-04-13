module Enemy (stepEnemy, collideEnemies) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB, eBB, solid)

stepEnemy :: Float -> [Tile] -> Enemy -> Enemy
stepEnemy dt sol e
  | not (eAlive e) = e { eTimer = eTimer e - dt }
  | otherwise = e { eX = ex', eVX = vx', eY = ey', eVY = vy' }
  where
    -- Horizontal movement with wall collision
    ex0   = eX e + eVX e * dt
    wallX = any (hit (ex0+ts/2, eY e+ts/2, ts*0.7, ts*0.7) . tBB) sol
    vx1   = if wallX then -eVX e else eVX e
    ex1   = if wallX then eX e else max 0 ex0

    -- Edge detection: check if there's ground ahead
    aheadX = ex1 + (if vx1 > 0 then ts else -ts)
    probe  = (aheadX+ts/2, eY e-ts/4, ts*0.5, ts*0.5)
    edge   = not (any (hit probe . tBB) sol)
    vx2   = if edge then -vx1 else vx1
    ex'   = ex1
    vx'   = vx2

    -- Vertical movement with gravity
    vy0   = eVY e + grav * dt
    ey0   = eY e + vy0 * dt
    onG   = any (hit (ex'+ts/2, ey0+ts/2, ts*0.5, ts*0.5) . tBB) sol
    (ey', vy') = if onG then (eY e, 0) else (ey0, vy0)

collideEnemies :: Mario -> [Enemy] -> Int -> (Mario,[Enemy],Int)
collideEnemies m es sc = foldr go (m,[],sc) es
  where
    go e (mario,acc,s)
      | not (eAlive e)                         = (mario, e:acc, s)
      | mInv mario > 0                         = (mario, e:acc, s)
      | not (hit (mBB mario) (eBB e))          = (mario, e:acc, s)
      | mY mario > eY e + ts*0.55, mVY mario < 50 =
          (mario { mVY=340 }, e { eAlive=False, eDead=True, eTimer=0.5 }:acc, s+100)
      | mState mario == Big =
          (mario { mState=Small, mInv=2 }, e:acc, s)
      | otherwise =
          (mario { mState=MDead, mVY=500, mVX=0 }, e:acc, s)