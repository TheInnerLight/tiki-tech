{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.KickOff where
import Football.Match
import Core (Log, Cache)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3), V2 (V2))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))
import Football.Pitch
import Football.Understanding.Shape (outOfPossessionFormationRelativeTo)
import Control.Monad (filterM)
import Football.Understanding.Space (isInOwnHalf)
import Football.Understanding.Team (fromTeamCoordinateSystem)
import Football.Locate2D (Locate2D(locate2D))
import Football.GameTime (gameTimeAddSeconds)

decideKickOffIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache) => Team -> Player -> m Player
decideKickOffIntention team player = do
  ball <- gameBall
  pitch' <- pitch
  time <- currentGameTime
  let kickOffLocation = V2 0 0
  releventPlayers <- filter (\p -> playerTeam p /= team  || (playerTeam p == team && playerNumber p /= 9)) <$> allPlayers
  notInHalf <- filterM (fmap not . isInOwnHalf) releventPlayers
  newIntention <-
    if playerTeam player /= team then do
      centreLoc <-  locate2D <$> fromTeamCoordinateSystem (playerTeam player) (V3 (-0.25*pitchLength pitch') 0 0)
      loc <- outOfPossessionFormationRelativeTo 1 1  player centreLoc
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    else if (playerNumber player == 9) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      pure $ RunToLocation kickOffLocation $ gameTimeAddSeconds time 0.1
    else if playerNumber player == 9 && null notInHalf then do
      -- FIX ME
      kickOffPass <- head <$> safestPassingOptions player
      pure $ TakeKickOffIntention (passTarget kickOffPass) kickOffLocation (passBallVector kickOffPass)
    else  do
      centreLoc <-  locate2D <$> fromTeamCoordinateSystem (playerTeam player) (V3 (-0.25*pitchLength pitch') 0 0)
      loc <- outOfPossessionFormationRelativeTo 1 1  player centreLoc
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
  pure player { playerIntention = newIntention }  