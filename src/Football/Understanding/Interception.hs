module Football.Understanding.Interception where

import Football.Pitch (Pitch, isInPitchBounds)
import Football.Player 
import Football.Ball
import Football.Types
import Linear (V3, Metric (distance))
import Football.Match (Match (pitch))
import Data.List (mapAccumR)
import Control.Lens ((&))
import Data.Maybe (fromMaybe)

zipAdj :: (a -> b) -> [a] -> [(a, b)]
zipAdj f x = zip x $ fmap f (tail x)

interceptionTimePlayerBallRK :: (Match m, Monad m) => Player -> Ball -> m Double
interceptionTimePlayerBallRK player ball = snd <$> interceptionInfoPlayerBallRK player ball

interceptionInfoPlayerBallRK :: (Match m, Monad m) => Player -> Ball -> m (V3 Double, Double)
interceptionInfoPlayerBallRK player ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.015
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && (distanceToTargetAfter dt (bpv', bmv') t' player > 0.5)
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)

data InterceptionData = InterceptionData
  { interceptionDataTime :: Double
  , interceptionDataDistance :: Double
  , interceptionDataBallLocation :: V3 Double
  , interceptionDataBallVector :: V3 Double
  } deriving (Eq, Ord, Show)

interceptionInfoPlayerBallRKI :: (Match m, Monad m) => Player -> Ball -> m [InterceptionData]
interceptionInfoPlayerBallRKI player ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.015

      folder (intData, intData2) (x:xs) | interceptionDataDistance intData2 < interceptionDataDistance intData = intData2 : x :xs
      folder (_, _) (x:xs) = x : xs
      folder (intData, intData2) [] | interceptionDataDistance intData2 < interceptionDataDistance intData = [intData2]
      folder (_, _) [] = []

      fastestAndOptimalInterception ls = 
        let ((intData,_):rest) =
              ls
              & fmap (\(t', (bpv', bmv')) -> 
                (InterceptionData t' (distanceToTargetAfter dt (bpv', bmv') t' player) bpv' bmv')
              )
              & zipAdj id
              & dropWhile (\((intData),d2) -> isInPitchBounds (interceptionDataBallLocation intData) pitch' && interceptionDataDistance intData > 0.5)
            remaining = takeWhile (\(intData',intData2') -> interceptionDataDistance intData2' < interceptionDataDistance intData' - 0.3  && isInPitchBounds (interceptionDataBallLocation intData') pitch' && interceptionDataTime intData' < 20) rest
        in intData : foldr folder [] remaining

  pure $ filter (\intData -> isInPitchBounds (interceptionDataBallLocation intData) pitch') $ fastestAndOptimalInterception $ rungeKutte (bpv, bmv) dt ballMotionEq



interceptionTimePlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m Double
interceptionTimePlayersBallRK players ball = snd <$> interceptionInfoPlayersBallRK players ball

interceptionInfoPlayersBallRK :: (Match m, Monad m) =>  [Player] -> Ball -> m (V3 Double, Double)
interceptionInfoPlayersBallRK players ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.015
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && all (\p -> distanceToTargetAfter dt (bpv', bmv') t' p > 0.5) players
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)

