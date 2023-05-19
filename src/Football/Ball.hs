module Football.Ball where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot), normalize)


data Ball = Ball
  { ballPositionVector :: V3 Double
  , ballMotionVector :: V3 Double
  }

updateBall :: Double -> Ball -> Ball
updateBall dt ball =
  let bpv = ballPositionVector ball
      bpv' = bpv + ballMotionVector ball / pure dt
      frictionMag = 0.5 * 9.81 * 0.43 / dt
      airResistanceMag = 0.43 * 0.5 * 1.293 * norm (ballMotionVector ball) ** 2.0 * 0.5 * (0.22 ** 2.0) * pi / dt
      resistanceMag = frictionMag + airResistanceMag

      ballUnitDirection = normalize $ ballMotionVector ball
      frictionVector = - pure resistanceMag * ballUnitDirection

      ballMotion' = ballMotionVector ball  + frictionVector
  in ball { ballPositionVector = bpv', ballMotionVector = ballMotion' }



