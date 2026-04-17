module PowerUp (bumpBlocks, stepPup, grabPups, pickCoins) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB)

-- | Hit a block from below.
--   • Small Mario    → always spawns a Mushroom
--   • Big / Fire     → spawns a Fire Flower (no horizontal movement)
--   • Brick + Big    → brick shatters (no power-up)
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
        QBlock ->
          let tls2  = map (\x -> if samePos x t then x { tType = Used } else x) tls
              -- Spawn position: just above the block
              bx    = fromIntegral (tCol t) * ts
              by    = fromIntegral (tRow t + 1) * ts + ts * 0.5
              -- Small Mario gets a Mushroom; powered-up Mario gets a Fire Flower
              pType = if mState m == Small then Mushroom else FireFlower
              pu0   = PUp bx by 120 True pType
          in (tls2, pu0:pus, sc + 50)
        Brick | mState m == Big || mState m == Fire ->
          let tls2 = filter (\x -> not (samePos x t)) tls
          in (tls2, pus, sc + 50)
        _ -> (tls, pus, sc)
    samePos a b = tCol a == tCol b && tRow a == tRow b

-- | Advance a power-up one frame.
--   Mushrooms slide to the right; Fire Flowers stay put (no horizontal velocity).
stepPup :: Float -> [Tile] -> PUp -> PUp
stepPup dt sol p
  | not (pAlive p) = p
  | otherwise = p { pX = x', pY = y', pVY = vy' }
  where
    hspd = case pType p of
             Mushroom   -> 80   -- slides rightward like the original
             FireFlower -> 0    -- stays on top of the block
             Star       -> 120  -- bouncy, handled same as mushroom for now
    x0  = pX p + hspd * dt
    y0  = pY p + pVY p * dt
    vy0 = pVY p + grav * dt
    onG = any (hit (x0 + ts/2, y0, ts*0.8, ts*0.8) . tBB) sol
    x'  = x0
    y'  = if onG then pY p else y0
    vy' = if onG then 0    else vy0

-- | Collect a power-up on contact.
--   Mushroom:   Small → Big  (+1000)
--   FireFlower: Small → Big, Big → Fire, Fire → no change  (+1000)
grabPups :: Mario -> [PUp] -> Int -> (Mario,[PUp],Int)
grabPups m ps sc = foldr go (m,[],sc) ps
  where
    go p (mario, acc, s)
      | not (pAlive p)                            = (mario, p:acc, s)
      | not (hit (mBB mario) (pupBB p))           = (mario, p:acc, s)
      | otherwise = (mario', p { pAlive = False } : acc, s + 1000)
      where
        mario' = applyPup (pType p) mario

    pupBB p = (pX p + ts/2, pY p, ts*0.85, ts*0.85)

applyPup :: PUpType -> Mario -> Mario
applyPup Mushroom   m = m { mState = if mState m == Small then Big  else mState m }
applyPup FireFlower m = m { mState = nextFireState (mState m) }
applyPup Star       m = m { mState = if mState m == Small then Big  else mState m }

nextFireState :: MS -> MS
nextFireState Small = Big
nextFireState Big   = Fire
nextFireState s     = s   -- Fire stays Fire; MDead unchanged

-- | Collect visible coins that Mario overlaps.
pickCoins :: BB -> [(Float,Float,Bool)] -> Int -> ([(Float,Float,Bool)],Int)
pickCoins mb cs sc = foldr go ([],sc) cs
  where
    go (x,y,True)  (acc,s) = ((x,y,True):acc, s)
    go (x,y,False) (acc,s)
      | hit mb (x,y,ts*0.5,ts*0.5) = ((x,y,True):acc, s + 200)
      | otherwise                   = ((x,y,False):acc, s)