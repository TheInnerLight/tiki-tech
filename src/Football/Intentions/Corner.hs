{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.Corner where
import Football.Match
import Core (Log, Cache, GetSystemTime (systemTimeNow))
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)

decideCornerIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, GetSystemTime m) => Team -> (Double, Double) -> Player -> m Player
decideCornerIntention team cornerLocation player = do
  ball <- gameBall
  closestPlayer <- head . sortOn (distance (V3 (fst cornerLocation) (snd cornerLocation) 0) . playerPositionVector ) <$> teamPlayers team
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if (playerNumber player == playerNumber closestPlayer) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      time <- systemTimeNow
      pure $ RunToLocation cornerLocation $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if playerNumber player == playerNumber closestPlayer then do
      -- FIX ME
      pure $ playerIntention player
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
  pure player { playerIntention = newIntention }  