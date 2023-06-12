{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.KickOff where
import Football.Match
import Core (Log, Cache, GetSystemTime (systemTimeNow))
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Types
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Data.List (sortOn)
import Linear (Metric(distance), V3 (V3))
import Football.Behaviours.Marking (positionalOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passBallVector))
import Football.Pitch (Pitch(pitchLength, pitchWidth))
import Football.Understanding.Shape (outOfPossessionFormationRelativeTo)
import Control.Monad (filterM)
import Football.Understanding.Space (isInOwnHalf)

decideKickOffIntention :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache, GetSystemTime m) => Team -> Player -> m Player
decideKickOffIntention team player = do
  ball <- gameBall
  pitch' <- pitch
  let kickOffLocation = (pitchLength pitch' / 2, pitchWidth pitch' / 2)
  releventPlayers <- filter (\p -> playerTeam p /= team  || (playerTeam p == team && playerNumber p /= 9)) <$> allPlayers
  notInHalf <- filterM (fmap not . isInOwnHalf) releventPlayers
  newIntention <-
    if playerTeam player /= team then do
      ad <- attackingDirection (playerTeam player)
      let centreLoc = case ad of
            AttackingLeftToRight -> (0.25*pitchLength pitch', 0.5*pitchWidth pitch')
            AttackingRightToLeft -> (0.75*pitchLength pitch', 0.5*pitchWidth pitch')
      loc <- outOfPossessionFormationRelativeTo 1 1  player centreLoc
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if (playerNumber player == 9) && distance (playerPositionVector player) (ballPositionVector ball) >= 0.5 then do
      time <- systemTimeNow
      pure $ RunToLocation kickOffLocation $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    else if playerNumber player == 9 && null notInHalf then do
      -- FIX ME
      kickOffPass <- head <$> safestPassingOptions player
      pure $ TakeKickOffIntention (passTarget kickOffPass) kickOffLocation (passBallVector kickOffPass)
    else  do
      ad <- attackingDirection (playerTeam player)
      let centreLoc = case ad of
            AttackingLeftToRight -> (0.25*pitchLength pitch', 0.5*pitchWidth pitch')
            AttackingRightToLeft -> (0.75*pitchLength pitch', 0.5*pitchWidth pitch')
      loc <- outOfPossessionFormationRelativeTo 1 1  player centreLoc
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
  pure player { playerIntention = newIntention }  