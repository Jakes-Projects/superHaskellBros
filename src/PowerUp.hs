module PowerUp (bumpBlocks, stepPup, grabPups, pickCoins, stepBrickAnims) where

import Constants (ts, grav)
import Types
import Physics (hit, mBB, tBB)

-- | Hit a block from below.
--   • Small Mario    → always spawns a Mushroom (or coin if QBlock has no pup)
--   • Big / Fire     → spawns a Fire Flower (no horizontal movement)
--   • Brick + Big    → brick shatters (no power-up)
--   Returns updated tiles, power-ups, score, new BrickAnims, and whether a brick was broken.
bumpBlocks :: Mario -> Float -> [Tile] -> [PUp] -> Int -> ([Tile],[PUp],Int,[BrickAnim],Bool)
bumpBlocks m vy tls pus sc
  | vy <= 0   = (tls, pus, sc, [], False)
  | otherwise = (tls', pus', sc', anims, broke)
  where
    (_,my,mw,mh) = mBB m
    headB = (mX m, my + mh/2 + 2, mw*0.65, 6)
    bumped = filter (hit headB . tBB) tls
    (tls', pus', sc', anims, broke) = case bumped of
      []    -> (tls, pus, sc, [], False)
      (t:_) -> case tType t of
        QBlock content ->
          let tls2  = map (\x -> if samePos x t then x { tType = Used } else x) tls
              bx    = fromIntegral (tCol t) * ts
              by    = fromIntegral (tRow t + 1) * ts + ts * 0.5
              bump  = BumpAnim (tCol t) (tRow t) 0.12
              coinPop = CoinPopAnim (fromIntegral (tCol t) * ts + ts/2)
                                    (fromIntegral (tRow t + 1) * ts)
                                    420 0.65
          in case content of
               QCoin ->
                 -- Always pops a coin regardless of Mario's state
                 (tls2, pus, sc + 200, [bump, coinPop], False)
               QPowerUp ->
                 -- Small Mario gets Mushroom; Big/Fire gets Fire Flower
                 let pType = if mState m == Small then Mushroom else FireFlower
                     pu0   = PUp bx by 120 True pType
                 in (tls2, pu0:pus, sc + 50, [bump], False)
        Brick | mState m == Big || mState m == Fire ->
          let tls2  = filter (\x -> not (samePos x t)) tls
              brk   = BreakAnim (tCol t) (tRow t) 0.15
          in (tls2, pus, sc + 50, [brk], True)
        Brick ->
          let bump = BumpAnim (tCol t) (tRow t) 0.12
          in (tls, pus, sc, [bump], False)
        _ -> (tls, pus, sc, [], False)
    samePos a b = tCol a == tCol b && tRow a == tRow b

-- | Advance all brick/block animations by one frame, discarding expired ones.
stepBrickAnims :: Float -> [BrickAnim] -> [BrickAnim]
stepBrickAnims dt = filter alive . map step
  where
    step (BumpAnim  c r t)       = BumpAnim  c r (t - dt)
    step (BreakAnim c r t)       = BreakAnim c r (t - dt)
    step (CoinPopAnim x y vy t)  =
      let y'  = y + vy * dt
          vy' = vy + grav * dt
      in CoinPopAnim x y' vy' (t - dt)
    alive (BumpAnim  _ _ t)      = t > 0
    alive (BreakAnim _ _ t)      = t > 0
    alive (CoinPopAnim _ _ _ t)  = t > 0

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