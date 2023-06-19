module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), normalize, project, V2 (V2))
import Football.Ball
import Data.List (sort)
import Data.Time.Clock.System (SystemTime)
import Football.Types
import Football.Maths
import Football.Locate2D (Locate2D(locate2D))

updatePlayer :: Double -> Player -> Player
updatePlayer dt player =
  let ppv = playerPositionVector player
      pmv = playerMotionVector player
      desired = 
        case playerDesiredLocation' player of
          Just (V2 desX desY) -> V3 desX desY 0
          Nothing             -> ppv
      desiredSpeed = min (playerSpeedMax (playerSpeed player)) $ (dt/2)*norm(desired - ppv)
      desiredMotion = pure desiredSpeed * normalize (desired - ppv)
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

playerControlCentre :: Double -> Player -> V3 Double
playerControlCentre time player =
  let pa = playerSpeedAcceleration (playerSpeed player)
      start = playerPositionVector player
  in start + pure ((1 - exp(-pa * time))/pa) * playerMotionVector player

-- use Fujimura-Sugihara model
distanceToTargetAfter :: (V3 Double, V3 Double) -> Double -> Player -> Double
distanceToTargetAfter (target, targetVector) t p =
  let 
      start = playerPositionVector p
      ms = playerSpeedMax (playerSpeed p)
      pa = playerSpeedAcceleration (playerSpeed p)
      -- vector difference between x(t) and A(t)
      st = target - start - pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      --st2 = start + pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      -- radius B(t)
      rt = (ms*(t - (1 - exp(-pa * t))/pa))

  in 
    -- fst (movingObjectAndPointClosestInterceptWithinTimeStep (-dt) (target, targetVector) st2) - rt
    norm st - rt

playerDesiredLocation' :: Player -> Maybe (V2 Double)
playerDesiredLocation' p =
  intentionToLocation (playerIntention p)
  where
    intentionToLocation (PassIntention _ loc _ _) = Just loc
    intentionToLocation (ThrowIntention _ loc _) = Just loc
    intentionToLocation (TakeCornerIntention _ loc _) = Just loc
    intentionToLocation (TakeGoalKickIntention _ loc _) = Just loc
    intentionToLocation (TakeKickOffIntention _ loc _) = Just loc
    intentionToLocation (ShootIntention _ loc _) = Just loc
    intentionToLocation (DribbleIntention loc _) = Just loc
    intentionToLocation (MoveIntoSpace loc _) = Just loc
    intentionToLocation (RunToLocation loc _) = Just loc
    intentionToLocation (ControlBallIntention loc _) = Just loc
    intentionToLocation (IntentionCooldown _) = Nothing
    intentionToLocation DoNothing = Nothing

intentionCooldown :: PlayerIntention -> Maybe GameTime
intentionCooldown (IntentionCooldown t)      = Just t
intentionCooldown (ControlBallIntention _ t) = Just t
intentionCooldown (MoveIntoSpace _ t)        = Just t
intentionCooldown (RunToLocation _ t)        = Just t
intentionCooldown (PassIntention _ _ _ t)    = Just t
intentionCooldown _                          = Nothing

isGoalKeeper :: Player -> Bool
isGoalKeeper player = playerNumber player == 1
