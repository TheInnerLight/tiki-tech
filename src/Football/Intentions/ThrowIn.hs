{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.ThrowIn where

import Control.Lens ((^.))
import Football.Match
import Core (Log, GetSystemTime (systemTimeNow), Cache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Linear (Metric(distance), V3 (V3), V2, R1 (_x), R2 (_y))
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Throw (throwOptions, ThrowDesirability (throwTarget, throwBallVector))
import Data.List (sortOn)


decideThrowInIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, GetSystemTime m) => Team -> V2 Double -> Player -> m Player
decideThrowInIntention team throwLocation player = do
  ball <- gameBall
  closestPlayer <- head . sortOn (distance (V3 (throwLocation ^. _x) (throwLocation ^. _y) 0) . playerPositionVector ) <$> teamPlayers team
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if (playerNumber player == playerNumber closestPlayer) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      time <- systemTimeNow
      pure $ RunToLocation throwLocation $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if playerNumber player == playerNumber closestPlayer then do
      -- just pick the first throw for now
      throw <- head <$> throwOptions player
      pure $ ThrowIntention (throwTarget throw) throwLocation (throwBallVector throw)
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
  pure player { playerIntention = newIntention }  

