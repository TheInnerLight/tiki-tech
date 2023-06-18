{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.Corner where
import Control.Lens ((^.))
import Football.Match
import Core (Log, Cache)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3), V2, R1 (_x), R2 (_y))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))
import Football.GameTime (gameTimeAddSeconds)

decideCornerIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache) => Team -> V2 Double -> Player -> m Player
decideCornerIntention team cornerLocation player = do
  ball <- gameBall
  time <- currentGameTime
  closestPlayer <- head . sortOn (distance (V3 (cornerLocation ^. _x) (cornerLocation ^. _y) 0) . playerPositionVector ) <$> teamPlayers team
  newIntention <-
    if playerTeam player /= team then do
      loc <- positionalOrientedZonalMark player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    else if (playerNumber player == playerNumber closestPlayer) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      pure $ RunToLocation cornerLocation $ gameTimeAddSeconds time 0.1
    else if playerNumber player == playerNumber closestPlayer then do
      -- FIX ME
      goalKickPass <- head <$> safestPassingOptions player
      pure $ TakeCornerIntention (passTarget goalKickPass) cornerLocation (passBallVector goalKickPass)
    else  do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc $ gameTimeAddSeconds time 0.1
  pure player { playerIntention = newIntention }  