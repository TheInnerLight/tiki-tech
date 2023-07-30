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

timeStepPlayerState :: Double -> PlayerState -> PlayerState
timeStepPlayerState dt playerState =
  let ppv = playerStatePositionVector playerState
      pmv = playerStateMotionVector playerState
      desired = 
        case playerDesiredLocation' playerState of
          Just (V2 desX desY) -> V3 desX desY 0
          Nothing             -> ppv
      desiredSpeed = min (playerSpeedMax . playerSpeed $ playerStatePlayer playerState) $ (dt/2)*norm(desired - ppv)
      desiredMotion = pure desiredSpeed * normalize (desired - ppv)
      direction = if norm(desiredMotion - pmv) > 0 then normalize (desiredMotion - pmv) else normalize desiredMotion
      (ppv', pmv') = rk2 (1.0/dt) (playerMotionEq direction (playerSpeed $ playerStatePlayer playerState)) (ppv, pmv)
  in playerState { playerStatePositionVector = ppv', playerStateMotionVector = pmv' }

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

playerControlCentre :: Double -> PlayerState -> V3 Double
playerControlCentre time playerState =
  let pa = playerSpeedAcceleration (playerSpeed $ playerStatePlayer playerState)
      start = playerStatePositionVector playerState
  in start + pure ((1 - exp(-pa * time))/pa) * playerStateMotionVector playerState

-- use Fujimura-Sugihara model
distanceToTargetAfter :: (V3 Double, V3 Double) -> Double -> PlayerState -> Double
distanceToTargetAfter (target, targetVector) t p =
  let 
      start = playerStatePositionVector p
      ms = playerSpeedMax (playerSpeed $ playerStatePlayer p)
      pa = playerSpeedAcceleration (playerSpeed $ playerStatePlayer p)
      -- vector difference between x(t) and A(t)
      st = target - start - pure ((1 - exp(-pa * t))/pa) * playerStateMotionVector p
      --st2 = start + pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      -- radius B(t)
      rt = (ms*(t - (1 - exp(-pa * t))/pa))

  in 
    -- fst (movingObjectAndPointClosestInterceptWithinTimeStep (-dt) (target, targetVector) st2) - rt
    norm st - rt

playerDesiredLocation' :: PlayerState -> Maybe (V2 Double)
playerDesiredLocation' p =
  intentionToLocation (playerStateIntention p)
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
    intentionToLocation (InterceptBallIntention loc _) = Just loc
    intentionToLocation (TackleIntention loc _) = Just loc
    intentionToLocation (IntentionCooldown _) = Nothing
    intentionToLocation DoNothing = Nothing

intentionCooldown :: PlayerIntention -> Maybe GameTime
intentionCooldown (IntentionCooldown t)      = Just t
intentionCooldown (ControlBallIntention _ t) = Just t
intentionCooldown (InterceptBallIntention _ t) = Just t
intentionCooldown (TackleIntention _ t) = Just t
intentionCooldown (MoveIntoSpace _ t)        = Just t
intentionCooldown (RunToLocation _ t)        = Just t
intentionCooldown (PassIntention _ _ _ t)    = Just t
intentionCooldown _                          = Nothing

isGoalKeeper :: Player -> Bool
isGoalKeeper player = playerNumber player == 1
