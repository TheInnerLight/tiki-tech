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
      dt = 0.03
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && (distanceToTargetAfter dt (bpv', bmv') t' player > 0.5)
      (t, (fbpv, fbmv)) = head $ drop 1 $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)

interceptionTimePlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m Double
interceptionTimePlayersBallRK players ball = snd <$> interceptionInfoPlayersBallRK players ball

interceptionInfoPlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m (V3 Double, Double)
interceptionInfoPlayersBallRK players ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.03
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && all (\p -> distanceToTargetAfter dt (bpv', bmv') t' p > 0.5) players
      (t, (fbpv, fbmv)) = head $ drop 1 $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)

