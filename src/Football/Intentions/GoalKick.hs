{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.GoalKick where

import Control.Lens ((^.))
import Football.Match
import Core (Log, Cache)
import Football.Understanding.Space.Data (CentresOfPlayCache, SpaceCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3), V2)
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))
import Football.GameTime (gameTimeAddSeconds)

decideGoalKickIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, Cache m SpaceCache) => Team -> V2 Double -> Player -> m Player
decideGoalKickIntention team goalKickLoc player = do
  ball <- gameBall
  time <- currentGameTime
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    else if (playerNumber player == 1) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      pure $ RunToLocation goalKickLoc $ gameTimeAddSeconds time 0.1
    else if playerNumber player == 1 then do
      -- FIX ME
      goalKickPass <- head <$> safestPassingOptions player
      pure $ TakeGoalKickIntention (passTarget goalKickPass) goalKickLoc (passBallVector goalKickPass)
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
  pure player { playerIntention = newIntention }  