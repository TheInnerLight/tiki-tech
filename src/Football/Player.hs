module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), normalize, project)
import Football.Ball
import Data.List (sort)
import Data.Time.Clock.System (SystemTime)
import Football.Types
import Football.Maths

updatePlayer :: Double -> Player -> Player
updatePlayer dt player =
  let ppv = playerPositionVector player
      pmv = playerMotionVector player
      desiredMotion = pure (playerSpeedMax $ playerSpeed player) * normalize (playerDesiredLocation player - ppv)
      direction = if norm(desiredMotion - pmv) > 0 then normalize (desiredMotion - pmv) else normalize desiredMotion
      (ppv', pmv') = rk2 (1.0/dt) (playerMotionEq direction (playerSpeed player)) (ppv, pmv)
  in player { playerPositionVector = ppv', playerMotionVector = pmv' }

playerMotionEq :: V3 Double -> PlayerSpeed -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
playerMotionEq direction pSpeed (ppv, pmv) =
  let acc = pure (playerSpeedMax pSpeed ) * direction  - pure (playerSpeedAcceleration pSpeed) * pmv
  in (pmv, acc)

rk2 :: Double -> ((V3 Double, V3 Double) -> (V3 Double, V3 Double)) -> (V3 Double, V3 Double) -> (V3 Double, V3 Double)
rk2 dt f (bp, bp')  = 
  (bp + pure dt*(k1 + 2.0*k2 + 2.0*k3 + k4)/6.0, bp' + pure dt*(k1' + 2.0*k2' + 2.0*k3' + k4')/6.0)
  where
      (k1, k1') = f (bp, bp')
      (k2, k2') = f (bp + pure (0.5*dt)*k1, bp' + pure (0.5*dt)*k1')
      (k3, k3') = f (bp + pure (0.5*dt)*k2, bp' + pure (0.5*dt)*k2')
      (k4, k4') = f (bp + pure (1.0*dt)*k3, bp' + pure (1.0*dt)*k3')

maxMag :: Double -> V3 Double -> V3 Double
maxMag m v =
  if norm v > m then
    normalize v * pure m
  else
    v

-- use Fujimura-Sugihara model
distanceToTargetAfter :: Double -> (V3 Double, V3 Double) -> Double -> Player -> Double
distanceToTargetAfter dt (target, targetVector) t p =
  let 
      start = playerPositionVector p
      ms = playerSpeedMax (playerSpeed p)
      pa = playerSpeedAcceleration (playerSpeed p)
      -- vector difference between x(t) and A(t)
      --st = target - start - pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      st2 = start + pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      -- radius B(t)
      rt = (ms*(t - (1 - exp(-pa * t))/pa))

  in 
    fst (movingObjectAndPointClosestInterceptWithinTimeStep (-dt) (target, targetVector) st2) - rt
    --norm st - rt

runTowardsLocation :: (Double, Double) -> Player -> Player
runTowardsLocation (x, y) player =
  let targetV = V3 x y 0
  in player { playerDesiredLocation = targetV }

stopMoving :: Player -> Player
stopMoving player =
  player { playerDesiredLocation = playerPositionVector player }


intentionCooldown :: PlayerIntention -> Maybe SystemTime
intentionCooldown (IntentionCooldown t) = Just t
intentionCooldown (ControlBallIntention _ t) = Just t
intentionCooldown _                     = Nothing

-- interceptionTimePlayerBall :: Player -> Ball -> Double
-- interceptionTimePlayerBall player ball =
--   case interceptionTime maxSpeed bpv (ballMotionVector ball) ppv (playerMotionVector player) of
--     Just d -> d
--     Nothing -> 20
--   where 
--     ppv = playerPositionVector player
--     bpv = ballPositionVector ball
--     maxSpeed = playerSpeedMax $ playerSpeed player

-- interceptionTime :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> Maybe Double
-- interceptionTime speed tpv tv ppv pv =
--   let a = quadrance tv - speed ** 2.0
--       b = 2.0 * dot (tpv - ppv) tv
--       c = quadrance (tpv - ppv)
--       root1 = (-b + sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
--       root2 = (-b - sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
--       ts = sort . filter (>0) $ [root1, root2]
--   in case ts of 
--     t : _ -> Just t
--     []    -> Nothing
  
-- interceptionVector :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double
-- interceptionVector speed tpv tv ppv pv =
--   case interceptionTime speed tpv tv ppv pv of
--     Just t -> normalize (tpv + pure t * tv - ppv)
--     Nothing -> tv

