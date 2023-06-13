{-# LANGUAGE FlexibleContexts #-}

module Football.Understanding.Interception where

import Football.Pitch (Pitch, isInPitchBounds)
import Football.Player 
import Football.Ball
import Football.Types
import Linear (V3, Metric (distance))
import Football.Match (Match (pitch))
import Data.List (mapAccumR, find, minimumBy)
import Control.Lens ((&))
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Football.Understanding.Interception.Data (InterceptionData(..), InterceptionDataCache)
import Core (Cache, cached)

zipAdj :: (a -> b) -> [a] -> [(a, b)]
zipAdj f x = zip x $ fmap f (tail x)

interceptionTimePlayerBallRK :: (Match m, Monad m) => Bool -> Player -> Ball -> m Double
interceptionTimePlayerBallRK includeOutOfBounds player ball = snd <$> interceptionInfoPlayerBallRK includeOutOfBounds player ball

interceptionInfoPlayerBallRK :: (Match m, Monad m) => Bool -> Player -> Ball -> m (V3 Double, Double)
interceptionInfoPlayerBallRK includeOutOfBounds player ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.015
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && (distanceToTargetAfter (bpv', bmv') t' player > 0.5)
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' || includeOutOfBounds then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)

-- outOfPlayTime :: (Match m, Monad m) =>  Ball -> m (V3 Double, Double)
-- outOfPlayTime ball = do
--   pitch' <- pitch
--   let bpv = ballPositionVector ball
--       bmv = ballMotionVector ball
--       dt = 0.015
--       dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch'
--       (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
--   if isInPitchBounds fbpv pitch' then
--     pure (fbpv, 1/0)
--   else 
--     pure (fbpv, t)

calcInterceptionInfoPlayerBallRKI :: (Match m, Monad m) => Player -> Ball -> m [InterceptionData]
calcInterceptionInfoPlayerBallRKI player ball = do
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
                (InterceptionData t' (distanceToTargetAfter (bpv', bmv') t' player) bpv' bmv')
              )
              & zipAdj id
              & dropWhile (\((intData),d2) -> isInPitchBounds (interceptionDataBallLocation intData) pitch' && interceptionDataDistance intData > 0.5)
            remaining = takeWhile (\(intData',intData2') -> interceptionDataDistance intData2' < interceptionDataDistance intData' - 0.3  && isInPitchBounds (interceptionDataBallLocation intData') pitch' && interceptionDataTime intData' < 20) rest
        in intData : foldr folder [] remaining

  pure $ filter (\intData -> isInPitchBounds (interceptionDataBallLocation intData) pitch') $ fastestAndOptimalInterception $ rungeKutte (bpv, bmv) dt ballMotionEq

interceptionInfoPlayerBallRKI :: (Match m, Monad m, Cache m InterceptionDataCache) => Player -> Ball -> m [InterceptionData]
interceptionInfoPlayerBallRKI player ball = 
  cached (uncurry calcInterceptionInfoPlayerBallRKI) (player, ball)

safestInterceptionOption :: [InterceptionData] -> Maybe InterceptionData
safestInterceptionOption icepts = 
  listToMaybe $ catMaybes [find (\x -> interceptionDataDistance x < -2.0) icepts, find (\x -> interceptionDataDistance x < -1.5) icepts, find (\x -> interceptionDataDistance x < -1.0) icepts,  find (\x -> interceptionDataDistance x < -0.5) icepts, listToMaybe icepts]

fastestInterceptionOption :: [InterceptionData] -> Maybe InterceptionData
fastestInterceptionOption (i:icepts) = 
  Just $ minimumBy (\x y -> compare (interceptionDataDistance x) (interceptionDataDistance y) ) (i:icepts)
fastestInterceptionOption [] = 
  Nothing

interceptionTimePlayersBallRK :: (Match m, Monad m) => Bool -> [Player] -> Ball -> m Double
interceptionTimePlayersBallRK includeOutOfBounds players ball = snd <$> interceptionInfoPlayersBallRK includeOutOfBounds players ball

interceptionInfoPlayersBallRK :: (Match m, Monad m) => Bool -> [Player] -> Ball -> m (V3 Double, Double)
interceptionInfoPlayersBallRK includeOutOfBounds players ball = do
  pitch' <- pitch
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      dt = 0.015
      dropOutOfRange (t', (bpv', bmv')) = isInPitchBounds bpv' pitch' && all (\p -> distanceToTargetAfter (bpv', bmv') t' p > 0.5) players
      (t, (fbpv, fbmv)) = head $ dropWhile dropOutOfRange $ rungeKutte (bpv, bmv) dt ballMotionEq
  if isInPitchBounds fbpv pitch' || includeOutOfBounds then
    pure (fbpv, t)
  else 
    pure (fbpv, 1/0)


