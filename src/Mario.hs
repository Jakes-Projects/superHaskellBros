module Mario (inputMario, inputMarioWater, tryJump, deathCheck) where

import Constants (walkSpd, runSpd, accelWalk, accelRun, jumpV)
import Types
import Physics (mBB)

-- | Nudge 'current' toward 'target' by at most 'maxStep', without overshooting.
moveToward :: Float -> Float -> Float -> Float
moveToward current target maxStep
  | current < target = min target (current + maxStep)
  | current > target = max target (current - maxStep)
  | otherwise        = target

inputMario :: Float -> KS -> Mario -> Mario
inputMario dt ks m
  | mState m == MDead = m
  | otherwise = m { mVX = vx, mFace = f, mCrouch = crouching, mSkidding = skidding }
  where
    crouching = kD ks && (mState m == Big || mState m == Fire) && mGround m
    prevVX    = mVX m

    -- Skidding: grounded, moving fast enough, opposite direction pressed.
    skidding = mGround m
            && not crouching
            && abs prevVX > 20
            && ((kL ks && prevVX > 0) || (kR ks && prevVX < 0))

    -- Acceleration rate depends on whether run is held.
    -- Use a higher rate for the walk band, lower for the run extension.
    accel = if abs prevVX < walkSpd then accelWalk else accelRun

    -- Target speed in the pressed direction (signed).
    targetSpd = if kRun ks then runSpd else walkSpd

    vx | crouching = prevVX * 0.78
       | skidding  = prevVX * 0.92        -- gentle bleed: slides noticeably before stopping
       | kL ks     = moveToward prevVX (-targetSpd) (accel * dt)
       | kR ks     = moveToward prevVX ( targetSpd) (accel * dt)
       | otherwise = prevVX * 0.78        -- no input: friction decay

    f | crouching = mFace m
      | kL ks     = -1
      | kR ks     =  1
      | otherwise = mFace m

-- | Underwater Mario controls.
-- Slower left/right movement, no crouching, and swimming is handled in Physics.hs.
inputMarioWater :: KS -> Mario -> Mario
inputMarioWater ks m
  | mState m == MDead = m
  | otherwise = m { mVX = vx, mFace = f, mCrouch = False }
  where
    spd = if kRun ks then 155 else 115

    vx | kL ks      = -spd
       | kR ks      =  spd
       | otherwise  = mVX m * 0.92

    f  | kL ks      = -1
       | kR ks      =  1
       | otherwise  = mFace m

tryJump :: Mario -> Mario
tryJump m
  -- Block jumping while crouching
  | mCrouch m                           = m
  | mGround m && mState m /= MDead      = m { mVY = jumpV, mGround = False }
  | otherwise                           = m

-- | Check whether Mario has fallen off the screen.
--   sx/sy are the level's starting position, used to re-spawn after losing a life.
--   Returns one phase step:
--     • 'Over' when lives run out
--     • 'Play' when a life is lost but lives remain (caller decrements the counter)
deathCheck :: Mario -> Int -> Float -> Float -> (Mario, Phase)
deathCheck m lv sx sy
  | mY m < -300 = (resetMario, if lv <= 0 then Over else Play)
  | otherwise   = (m, Play)
  where
    resetMario = m { mX       = sx
                   , mY       = sy
                   , mVX      = 0
                   , mVY      = 0
                   , mState   = Small
                   , mFace    = 1
                   , mInv     = 0
                   , mFireCool = 0
                   , mCrouch    = False
                   , mGround    = False
                   , mSliding   = False
                   , mSkidding  = False
                   }