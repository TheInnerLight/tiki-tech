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
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))
import Football.GameTime (gameTimeAddSeconds)
import Football.Understanding.PositionalPlay (ppInPossessionDesiredPosition)

decideGoalKickIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, Cache m SpaceCache) => TeamId -> V2 Double -> Player -> m PlayerIntention
decideGoalKickIntention team goalKickLoc player = do
  ball <- gameBall
  time <- currentGameTime
  playerState <- getPlayerState player
  if playerTeamId player /= team then do
    loc <- positionalOrientedZonalMark player
    pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
  else if (playerNumber player == 1) && distance (playerStatePositionVector playerState) (ballPositionVector ball) >= 0.5 then do
    pure $ RunToLocation goalKickLoc $ gameTimeAddSeconds time 0.1
  else if playerNumber player == 1 then do
    -- FIX ME
    goalKickPass <- head <$> safestPassingOptions player
    pure $ TakeGoalKickIntention (passTarget goalKickPass) goalKickLoc (passBallVector goalKickPass)
  else  do
    nearbySpace <- ppInPossessionDesiredPosition player
    targetLoc <- clampPitch nearbySpace
    pure $ MoveIntoSpace targetLoc time
  