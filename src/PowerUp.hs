module PowerUp (bumpBlocks, stepPup, grabPups, pickCoins) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB)

bumpBlocks :: Mario -> Float -> [Tile] -> [PUp] -> Int -> ([Tile],[PUp],Int)
bumpBlocks m vy tls pus sc
  | vy <= 0   = (tls, pus, sc)
  | otherwise = (tls', pus', sc')
  where
    (_,my,mw,mh) = mBB m
    headB = (mX m, my + mh/2 + 2, mw*0.65, 6)
    bumped = filter (hit headB . tBB) tls
    (tls', pus', sc') = case bumped of
      []    -> (tls, pus, sc)
      (t:_) -> case tType t of
        QBlock -> spawn t
        Hidden -> spawn t
        Brick | mState m == Big ->
          let tls2 = filter (\x -> not (samePos x t)) tls
          in (tls2, pus, sc + 50)
        _ -> (tls, pus, sc)

    spawn t =
      let tls2 = map (\x -> if samePos x t then x { tType = Used } else x) tls
          ptype = case (tCol t, tRow t) of
                    (16,3) -> Mushroom
                    (22,7) -> OneUp      -- hidden 1‑up
                    (79,7) -> Starman    -- star block
                    _      -> Mushroom
          pu0 = PUp (fromIntegral (tCol t) * ts)
                    (fromIntegral (tRow t + 1) * ts + ts * 0.5)
                    120 True ptype
      in (tls2, pu0:pus, sc + 50)

    samePos a b = tCol a == tCol b && tRow a == tRow b

stepPup :: Float -> [Tile] -> PUp -> PUp
stepPup dt sol p
  | not (pAlive p) = p
  | otherwise = p { pX=x', pY=y', pVY=vy' }
  where
    x0  = pX p + 80*dt
    y0  = pY p + pVY p * dt
    vy0 = pVY p + grav*dt
    onG = any (hit (x0+ts/2, y0, ts*0.8, ts*0.8) . tBB) sol
    x'  = x0
    y'  = if onG then pY p else y0
    vy' = if onG then 0 else vy0

grabPups :: Mario -> [PUp] -> Int -> (Mario, [PUp], Int, Int)   -- returns (Mario, pups, score, livesDelta)
grabPups m ps sc = foldr go (m, [], sc, 0) ps
  where
    go p (mario, acc, s, livesDelta)
      | not (pAlive p) = (mario, p:acc, s, livesDelta)
      | hit (mBB mario) (pX p+ts/2, pY p, ts*0.85, ts*0.85) =
          let (newMario, addScore, extraLives) = applyPUp mario (pType p)
          in (newMario, p { pAlive=False }:acc, s + addScore, livesDelta + extraLives)
      | otherwise = (mario, p:acc, s, livesDelta)

    applyPUp mario Mushroom =
      if mState mario == Small then (mario { mState = Big }, 1000, 0) else (mario, 1000, 0)
    applyPUp mario FireFlower =
      (mario { mState = Fire }, 1000, 0)
    applyPUp mario Starman =
      (mario { mState = Star 10.0, mInv = 10.0 }, 1000, 0)
    applyPUp mario OneUp =
      (mario, 0, 1)

pickCoins :: BB -> [(Float,Float,Bool)] -> Int -> ([(Float,Float,Bool)],Int)
pickCoins mb cs sc = foldr go ([],sc) cs
  where
    go (x,y,True)  (acc,s) = ((x,y,True):acc, s)
    go (x,y,False) (acc,s)
      | hit mb (x,y,ts*0.5,ts*0.5) = ((x,y,True):acc, s+200)
      | otherwise                   = ((x,y,False):acc, s)