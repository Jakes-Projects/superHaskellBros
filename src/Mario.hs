module Mario (inputMario, inputMarioWater, tryJump, deathCheck) where

import Constants (walkSpd, runSpd, jumpV)
import Types
import Physics (mBB)

inputMario :: KS -> Mario -> Mario
inputMario ks m
  | mState m == MDead = m
  | otherwise = m { mVX = vx, mFace = f, mCrouch = crouching }
  where
    -- Crouch when down is held and powered up. No mGround check here —
    -- that caused a circular dependency with physicsMario shrinking the BB.
    crouching = kD ks && (mState m == Big || mState m == Fire) && mGround m
    spd = if kRun ks then runSpd else walkSpd
    -- No horizontal movement while crouching
    vx | crouching  = mVX m * 0.78   -- decelerate to a stop
       | kL ks      = -spd
       | kR ks      =  spd
       | otherwise  = mVX m * 0.78
    -- Keep facing direction while crouching
    f  | crouching  = mFace m
       | kL ks      = -1
       | kR ks      =  1
       | otherwise  = mFace m

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
                   , mCrouch  = False
                   , mGround  = False
                   , mSliding = False
                   }