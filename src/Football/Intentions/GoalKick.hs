{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.GoalKick where

import Control.Lens ((^.))
import Football.Match
import Core (Log, Cache, GetSystemTime (systemTimeNow))
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3), V2)
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))

decideGoalKickIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, GetSystemTime m) => Team -> V2 Double -> Player -> m Player
decideGoalKickIntention team goalKickLoc player = do
  ball <- gameBall
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if (playerNumber player == 1) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      time <- systemTimeNow
      pure $ RunToLocation goalKickLoc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if playerNumber player == 1 then do
      -- FIX ME
      goalKickPass <- head <$> safestPassingOptions player
      pure $ TakeGoalKickIntention (passTarget goalKickPass) goalKickLoc (passBallVector goalKickPass)
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
  pure player { playerIntention = newIntention }  