module Mario (inputMario, tryJump, deathCheck) where

import Constants (walkSpd, runSpd, jumpV)
import Types
import Physics (mBB)

inputMario :: KS -> Mario -> Mario
inputMario ks m
  | mState m == MDead = m
  | otherwise = m { mVX = vx, mFace = f }
  where
    spd = if kRun ks then runSpd else walkSpd
    vx | kL ks    = -spd
       | kR ks    =  spd
       | otherwise = mVX m * 0.78
    f  | kL ks    = -1
       | kR ks    =  1
       | otherwise = mFace m

tryJump :: Mario -> Mario
tryJump m
  | mGround m && mState m /= MDead = m { mVY = jumpV, mGround = False }
  | otherwise = m

deathCheck :: Mario -> Int -> (Mario, Phase)
deathCheck m lv
  | mY m < -300 = (initMarioPlaceholder, if lv <= 1 then Over else Play)
  | otherwise   = (m, Play)
  where
    initMarioPlaceholder = m { mX = 96, mY = 48, mVX = 0, mVY = 0, mState = Small, mFace = 1, mInv = 0 }