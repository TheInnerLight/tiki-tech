module Football.Understanding.Interception where

import Football.Pitch (Pitch, isInPitchBounds)
import Football.Player 
import Football.Ball
import Linear (V3)

interceptionTimePlayerBallRK :: Pitch -> Player -> Ball -> Double
interceptionTimePlayerBallRK pitch player ball = snd $ interceptionInfoPlayerBallRK pitch player ball

interceptionInfoPlayerBallRK :: Pitch -> Player -> Ball -> (V3 Double, Double)
interceptionInfoPlayerBallRK pitch player ball =
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      droppity (t', (bpv', bmv')) = isInPitchBounds bpv' pitch && (distanceToTargetAfter bpv' t' player > 0.5)
      (t, (fbpv, fbmv)) = head $ dropWhile droppity $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  in
    if isInPitchBounds fbpv pitch then
      (fbpv, t)
    else 
      (fbpv, 1/0)

interceptionTimePlayersBallRK :: Pitch -> [Player] -> Ball -> Double
interceptionTimePlayersBallRK pitch players ball = snd $ interceptionInfoPlayersBallRK pitch players ball

interceptionInfoPlayersBallRK :: Pitch -> [Player] -> Ball -> (V3 Double, Double)
interceptionInfoPlayersBallRK pitch players ball =
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      droppity (t', (bpv', bmv')) = isInPitchBounds bpv' pitch && all (\p -> distanceToTargetAfter bpv' t' p > 0.5) players
      (t, (fbpv, fbmv)) = head $ dropWhile droppity $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  in
    if isInPitchBounds fbpv pitch then
      (fbpv, t)
    else 
      (fbpv, 1/0)

