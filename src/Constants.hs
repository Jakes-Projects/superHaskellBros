module Constants where

ts :: Float
ts = 32

grav :: Float
grav = -1400

jumpV :: Float
jumpV = 630

walkSpd, runSpd :: Float
walkSpd = 180
runSpd  = 300

-- | Ground acceleration: how fast Mario ramps up to walk/run speed (px/s²).
-- accelWalk covers the 0→walkSpd ramp; accelRun the walkSpd→runSpd extension.
accelWalk, accelRun :: Float
accelWalk = 900    -- reaches walkSpd (180) from rest in ~0.2s
accelRun  = 600    -- reaches runSpd  (300) from walkSpd in ~0.2s

sW, sH :: Int
sW = 800
sH = 600