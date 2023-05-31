module Football.Understanding.Interception where

import Football.Pitch (Pitch, isInPitchBounds)
import Football.Player 
import Football.Ball
import Football.Types
import Linear (V3)
import Football.Match (Match (pitch))

interceptionTimePlayerBallRK :: (Match m, Monad m) => Player -> Ball -> m Double
interceptionTimePlayerBallRK player ball = snd <$> interceptionInfoPlayerBallRK player ball

interceptionInfoPlayerBallRK :: (Match m, Monad m) => Player -> Ball -> m (V3 Double, Double)
interceptionInfoPlayerBallRK player ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && (distanceToTargetAfter bpv' t' player > 0.5)
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv + fbmv*0.03, t)
  else 
    pure (fbpv, 1/0)

interceptionTimePlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m Double
interceptionTimePlayersBallRK players ball = snd <$> interceptionInfoPlayersBallRK players ball

interceptionInfoPlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m (V3 Double, Double)
interceptionInfoPlayersBallRK players ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && all (\p -> distanceToTargetAfter bpv' t' p > 0.5) players
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv + fbmv*0.03, t)
  else 
    pure (fbpv, 1/0)

