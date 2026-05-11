module Physics (hit, mBB, tBB, eBB, solid, physicsMario, physicsMarioWater) where

import Constants (ts, grav)
import Types

hit :: BB -> BB -> Bool
hit (ax,ay,aw,ah) (bx,by,bw,bh) =
  abs(ax-bx) < (aw+bw)/2 && abs(ay-by) < (ah+bh)/2

-- Fire Mario is the same physical size as Big Mario (2 tiles tall).
mBB :: Mario -> BB
mBB m = (mX m, mY m, ts*0.78, if mState m == Big || mState m == Fire then ts*2 else ts)

tBB :: Tile -> BB
tBB t = (fromIntegral (tCol t)*ts + ts/2,
         fromIntegral (tRow t)*ts + ts/2, ts, ts)

eBB :: Enemy -> BB
eBB e
  | eType e == Bowser =
      -- Bowser is larger than normal enemies.
      -- eX/eY is his lower-left-ish position, so center the hitbox around his body.
      (eX e + ts, eY e + ts, ts * 1.55, ts * 1.65)

  | eType e == Piranha =
      -- Piranha sprite is 48 wide × 72 tall, rendered centered at
      -- (eX + ts/2, eY + 36). The default ts*0.78 box only covers the
      -- bottom ~1/3 of the plant, so fireballs (and stomps) routinely
      -- pass through the head without registering. Match the sprite.
      (eX e + ts / 2, eY e + 36, ts * 1.25, ts * 2)

  | otherwise =
      (eX e + ts / 2, eY e + ts / 2, ts * 0.78, ts * 0.78)

solid :: TType -> Bool
solid Ground      = True
solid Brick       = True
solid (QBlock _)   = True
solid Used        = True
solid PipeTop     = True
solid Pipe        = True
solid PipeR       = True
solid FlagBase    = True
solid Castle      = True
solid Axe         = False
solid FirebarTile = False
solid Step        = True
solid Coral       = True
solid _           = False

-- | Sub-stepping physics: number of steps proportional to speed
physicsMario :: Float -> [Tile] -> Mario -> Mario
physicsMario dt sol m0 = foldl (\m _ -> advanceStep m) m0 [1..steps]
  where
    -- Determine number of sub-steps
    maxDist = ts * 0.4
    vxAbs = abs (mVX m0)
    vyAbs = abs (mVY m0)
    steps = max 1 (ceiling (max (vxAbs * dt / maxDist) (vyAbs * dt / maxDist)))
    dt' = dt / fromIntegral steps

    advanceStep :: Mario -> Mario
    advanceStep m = m { mX = nx, mY = ny, mVX = nvx, mVY = nvy, mGround = grounded }
      where
        (_, _, cw, ch) = mBB m
        halfW = cw * 0.45
        halfH = ch * 0.5

        tLeft  t = fromIntegral (tCol t) * ts
        tRight t = tLeft t + ts
        tBot   t = fromIntegral (tRow t) * ts
        tTop   t = tBot t + ts

        mLeft  x = x - halfW
        mRight x = x + halfW
        mBottom y = y - halfH
        mTop    y = y + halfH

        overlapXY x y t =
          mRight x > tLeft t && mLeft x < tRight t &&
          mTop   y > tBot   t && mBottom y < tTop  t

        vy1 = max (-900) (mVY m + grav * dt')

        x1 = mX m + mVX m * dt'
        x1c = max halfW x1
        xCols = filter (overlapXY x1c (mY m)) sol
        (nx, nvx)
          | null xCols = (x1c, mVX m)
          | mVX m > 0  =
              let blocking = minimum (map tLeft xCols)
              in (blocking - halfW - 0.5, 0)
          | mVX m < 0  =
              let blocking = maximum (map tRight xCols)
              in (blocking + halfW + 0.5, 0)
          | otherwise  = (mX m, 0)

        y1 = mY m + vy1 * dt'
        yCols = filter (overlapXY nx y1) sol
        (ny, nvy, grounded)
          | null yCols = (y1, vy1, False)
          | vy1 <= 0 =
              let blocking = maximum (map tTop yCols)
              in (blocking + halfH, 0, True)
          | otherwise =
              let blocking = minimum (map tBot yCols)
              in (blocking - halfH, -50, False)

-- | Underwater Mario physics.
-- Lower gravity, slower fall speed, and holding jump/swim gives upward force.
physicsMarioWater :: Float -> KS -> [Tile] -> Mario -> Mario
physicsMarioWater dt ks sol m0 = foldl (\m _ -> advanceStep m) m0 [1..steps]
  where
    maxDist = ts * 0.4
    vxAbs = abs (mVX m0)
    vyAbs = abs (mVY m0)
    steps = max 1 (ceiling (max (vxAbs * dt / maxDist) (vyAbs * dt / maxDist)))
    dt' = dt / fromIntegral steps
    waterGrav :: Float
    waterGrav = -135

    swimAccel :: Float
    swimAccel = 700

    maxFall :: Float
    maxFall = -165

    maxRise :: Float
    maxRise = 180

    advanceStep :: Mario -> Mario
    advanceStep m = m { mX = nx, mY = ny, mVX = nvx, mVY = nvy, mGround = grounded }
      where
        (_, _, cw, ch) = mBB m
        halfW = cw * 0.45
        halfH = ch * 0.5

        tLeft  t = fromIntegral (tCol t) * ts
        tRight t = tLeft t + ts
        tBot   t = fromIntegral (tRow t) * ts
        tTop   t = tBot t + ts

        mLeft x   = x - halfW
        mRight x  = x + halfW
        mBottom y = y - halfH
        mTop y    = y + halfH

        overlapXY x y t =
          mRight x > tLeft t && mLeft x < tRight t &&
          mTop y   > tBot t  && mBottom y < tTop t

        swimForce = if kJ ks then swimAccel else 0
        vy1 = max maxFall (min maxRise (mVY m + (waterGrav + swimForce) * dt'))

        x1 = mX m + mVX m * dt'
        x1c = max halfW x1
        xCols = filter (overlapXY x1c (mY m)) sol

        (nx, nvx)
          | null xCols = (x1c, mVX m)
          | mVX m > 0  =
              let blocking = minimum (map tLeft xCols)
              in (blocking - halfW - 0.5, 0)
          | mVX m < 0  =
              let blocking = maximum (map tRight xCols)
              in (blocking + halfW + 0.5, 0)
          | otherwise  = (mX m, 0)

        y1 = mY m + vy1 * dt'
        yCols = filter (overlapXY nx y1) sol
        waterCeilY = 400 - halfH

        (ny, nvy, grounded)
          | not (null yCols) && vy1 > 0 =
              let blocking = minimum (map tBot yCols)
              in (blocking - halfH, -40, False)
          | not (null yCols) =
              let blocking = maximum (map tTop yCols)
              in (blocking + halfH, 0, True)
          | y1 > waterCeilY = (waterCeilY, min 0 vy1, False)
          | otherwise        = (y1, vy1, False)              