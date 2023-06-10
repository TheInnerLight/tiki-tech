{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.ThrowIn where
import Football.Match
import Core (Log, GetSystemTime (systemTimeNow), Cache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Linear (Metric(distance))
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (optimalNearbySpace)


decideThrowInIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, GetSystemTime m) => Team -> (Double, Double) -> Player -> m Player
decideThrowInIntention team throwLocation player = do
  ball <- gameBall
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if (playerNumber player == 3) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      time <- systemTimeNow
      pure $ RunToLocation throwLocation $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if playerNumber player == 3 then do
      -- take the throw-in somehow
      pure $ playerIntention player
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
  pure player { playerIntention = newIntention }  

