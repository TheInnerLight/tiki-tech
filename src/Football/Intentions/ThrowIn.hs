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


decideThrowInIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, Cache m SpaceCache) => TeamId -> V2 Double -> Player -> m PlayerIntention
decideThrowInIntention team throwLocation player = do
  ball <- gameBall
  time <- currentGameTime
  playerState <- getPlayerState player
  closestPlayer <- head . sortOn (distance (V3 (throwLocation ^. _x) (throwLocation ^. _y) 0) . playerStatePositionVector ) <$> teamPlayers team
  if playerTeamId player /= team then do
    loc <- positionalOrientedZonalMark player
    pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
  else if (player == playerStatePlayer closestPlayer) && distance (playerStatePositionVector playerState) (ballPositionVector ball) >= 0.5 then do
    pure $ RunToLocation throwLocation $ gameTimeAddSeconds time 0.1
  else if player == playerStatePlayer closestPlayer then do
    -- just pick the first throw for now
    throw <- head <$> throwOptions player
    pure $ ThrowIntention (throwTarget throw) throwLocation (throwBallVector throw)
  else  do
    nearbySpace <- optimalNearbySpace player
    targetLoc <- clampPitch nearbySpace
    pure $ MoveIntoSpace targetLoc time


