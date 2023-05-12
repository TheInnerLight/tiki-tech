module Football.Ball where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot))


data Ball = Ball
  { ballPosition :: (Double, Double)
  , ballMotionVector :: V3 Double
  }

updateBall :: Double -> Ball -> Ball
updateBall dt ball =
  let (bx, by) = ballPosition ball
      bx' = bx + (ballMotionVector ball ^. _x / dt)
      by' = by + (ballMotionVector ball ^. _y / dt)
      frictionMag = 0.5 * 9.81 * 0.43 / dt
      airResistanceMag = 0.5 * 1.293 * norm (ballMotionVector ball) ** 2.0 * 0.9 * (0.22 ** 2.0) * pi / dt
      resistanceMag = frictionMag + airResistanceMag
      ballUnitDirection = signorm $ ballMotionVector ball
      frictionVector = V3 (- resistanceMag * ballUnitDirection ^. _x) (- resistanceMag * ballUnitDirection ^. _y) (- resistanceMag * ballUnitDirection ^. _z)
      ballMotion' = ballMotionVector ball + frictionVector
  in ball { ballPosition = (bx', by'), ballMotionVector = ballMotion' }

-- kickBall :: (Double, Double) -> Ball -> Ball
-- kickBall (targetX, targetY) ball = 
--   let (bx, by) = ballPosition ball
--       ballDirection = signorm $ V3 (targetX - bx) (targetY - by) 0

--   in ball { ballMotionVector = ballDirection * 20 }




