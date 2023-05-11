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
      ballUnitDirection = signorm $ ballMotionVector ball
      frictionVector = V3 (- frictionMag * ballUnitDirection ^. _x) (- frictionMag * ballUnitDirection ^. _y) (- frictionMag * ballUnitDirection ^. _z)
      ballMotion' = ballMotionVector ball + frictionVector
  in ball { ballPosition = (bx', by'), ballMotionVector = ballMotion' }

kickBall :: Double -> (Double, Double) -> Ball -> Ball
kickBall dt (targetX, targetY) ball = 
  let (bx, by) = ballPosition ball
      bx' = bx + (ballMotionVector ball ^. _x / dt)
      by' = by + (ballMotionVector ball ^. _y / dt)
      ballUnitDirection = signorm $ ballMotionVector ball
      ballDirection = signorm $ V3 (targetX - bx) (targetY - by) 0

  in ball { ballMotionVector = ballDirection * 20 }




