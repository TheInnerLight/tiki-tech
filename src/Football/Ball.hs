module Football.Ball where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot), normalize)
import Data.List (unfoldr)
import GHC.IO (unsafePerformIO)
import Control.Concurrent (threadDelay)
import Football.Types

updateBall :: Double -> Ball -> Ball
updateBall dt ball =
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      --(dbpv, dbmv) = ballMotionEq (1.0/dt) (bpv, bmv)
      (bpv', bmv') = rk (1.0/dt) ballMotionEq (bpv, bmv)
  in ball { ballPositionVector = bpv', ballMotionVector = bmv'}


ballMotionEq :: Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
ballMotionEq dt (bpv, bmv) =
  let ballUnitDirection = normalize bmv
      resistanceMag = min (norm bmv) (frictionMag + airResistanceMag)
  in (pure dt * bmv, - pure resistanceMag * ballUnitDirection)
  where 
    frictionMag = 0.5 * 9.81 * 0.43 * dt
    airResistanceMag = 0.43 * 0.5 * 1.293 * norm bmv ** 2.0 * 0.5 * (0.22 ** 2.0) * pi * dt

rk :: Double -> (Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)) -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
rk dt f (bp, bp')  = 
  (bp + (k1 + 2.0*k2 + 2.0*k3 + k4)/6.0, bp' + (k1' + 2.0*k2' + 2.0*k3' + k4')/6.0)
  where
      (k1, k1') = f dt (bp, bp')
      (k2, k2') = f (0.5*dt) (bp + pure (0.5*dt)*k1, bp' + pure (0.5*dt)*k1')
      (k3, k3') = f (0.5*dt) (bp + pure (0.5*dt)*k2, bp' + pure (0.5*dt)*k2')
      (k4, k4') = f (1.0*dt) (bp + pure (1.0*dt)*k3, bp' + pure (1.0*dt)*k3')

rungeKutte :: (V3 Double, V3 Double) -> Double -> (Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)) -> [(Double, (V3 Double, V3 Double))]
rungeKutte initial dt f = 
  unfoldr unfolder (0, initial)
  where
    unfolder (t, st) = 
      let rk' = rk dt f st
      in Just ((t+dt, rk'), (t+dt, rk'))



