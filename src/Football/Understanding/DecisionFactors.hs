{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Football.Understanding.DecisionFactors where

import Football.Player (Player (playerPositionVector, playerMotionVector, playerTeam, playerNumber), interceptionInfoPlayerBallRK, interceptionTimePlayersBallRK)
import Football.Match
import Football.Locate2D (Locate2D(locate2D))
import Football.Ball (Ball(..))
import Control.Monad (filterM, join)
import Linear (Metric(norm))
import Data.Foldable (find, Foldable (foldMap'))
import Data.Maybe (maybe, isJust)
import Data.List (sort)

data DecisionFactors
  = ClosestPlayerToBall (Double, Double) Double
  | HasControlOfBall
  | TeammateInPossession Player
  deriving (Eq, Ord, Show)



checkClosestPlayer :: (Match m, Monad m) => Player -> m [DecisionFactors]
checkClosestPlayer player = do
  ball <- gameBall
  teamPlayers' <- teammates player
  let (iceptLoc3D, iceptTime) = interceptionInfoPlayerBallRK player ball
  let iceptLoc = locate2D iceptLoc3D
  let noCloserPlayers = interceptionTimePlayersBallRK teamPlayers' ball >= iceptTime
  if noCloserPlayers then
    pure [ClosestPlayerToBall iceptLoc iceptTime]
  else
    pure []

checkTeammateInPossession :: (Match m, Monad m) => Player -> m [DecisionFactors]
checkTeammateInPossession player = do
  ball <- gameBall
  lastTouch <- lastTouchOfBall
  case lastTouch of
    Just lp | playerTeam lp == playerTeam player && playerNumber lp /= playerNumber player -> pure [TeammateInPossession lp]
    _ -> pure []

  -- teamPlayers' <- teammates player
  -- teammatesInKickingRange <- filterM canKick teamPlayers'
  -- let teammateInPossession = find (\p -> norm (playerMotionVector p - ballMotionVector ball) < 1.0 ) teammatesInKickingRange
  -- maybe (pure []) (\p -> pure [TeammateInPossession p]) teammateInPossession

checkInPossession :: (Match m, Monad m) => Player -> m [DecisionFactors]
checkInPossession player = do
  ball <- gameBall
  canKick' <- canKick player
  if (canKick' && norm (playerMotionVector player - ballMotionVector ball) < 1) then
    pure [HasControlOfBall]
  else 
    pure []

calculateDecisionFactors :: (Match m, Monad m) => Player -> m [DecisionFactors]
calculateDecisionFactors player = do
  (\l1 l2 l3 -> l2 ++ l1 ++ l3) <$> checkClosestPlayer player <*> checkTeammateInPossession player <*> checkInPossession player



