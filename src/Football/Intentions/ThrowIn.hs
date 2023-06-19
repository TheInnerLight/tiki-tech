{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.ThrowIn where

import Control.Lens ((^.))
import Football.Match
import Core (Log, Cache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Understanding.Space.Data (CentresOfPlayCache, SpaceCache)
import Linear (Metric(distance), V3 (V3), V2, R1 (_x), R2 (_y))
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Throw (throwOptions, ThrowDesirability (throwTarget, throwBallVector))
import Data.List (sortOn)
import Football.GameTime (gameTimeAddSeconds)


decideThrowInIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, Cache m SpaceCache) => Team -> V2 Double -> Player -> m Player
decideThrowInIntention team throwLocation player = do
  ball <- gameBall
  time <- currentGameTime
  closestPlayer <- head . sortOn (distance (V3 (throwLocation ^. _x) (throwLocation ^. _y) 0) . playerPositionVector ) <$> teamPlayers team
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    else if (playerNumber player == playerNumber closestPlayer) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      pure $ RunToLocation throwLocation $ gameTimeAddSeconds time 0.1
    else if playerNumber player == playerNumber closestPlayer then do
      -- just pick the first throw for now
      throw <- head <$> throwOptions player
      pure $ ThrowIntention (throwTarget throw) throwLocation (throwBallVector throw)
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
  pure player { playerIntention = newIntention }  

