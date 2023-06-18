module Football.Behaviours.Dribble where

import Linear (V3(..), normalize, V2 (V2))
import Football.Match
import Core (Log)
import Football.Player
import Football.Ball
import Football.Behaviours.FindSpace (nearestSpace)
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Understanding.Interception (interceptionTimePlayerBallRK, interceptionTimePlayersBallRK)
import Football.Behaviours.Kick (motionVectorForDribble)
import Football.Locate2D (Locate2D(locate2D))
import Football.Types
import Data.List (sortOn)
import qualified Data.Ord
import Football.Pitch (rightGoalLine, leftGoalLine)
import qualified Statistics.Distribution.Normal as ND
import Statistics.Distribution (Distribution(cumulative))

data DribbleTarget 
  = DribbleAwayFromOpponents (V2 Double)
  | DribbleTowardsTouchline (V3 Double)
  | DribbleTowardsGoal (V3 Double)
  deriving Show

data DribbleDesirability =
  DribbleDesirability
    { dribbleTarget :: !DribbleTarget
    , dribbleDirection :: !(V3 Double)
    , dribbleSafetyCoeff :: !Double
    , dribbleXGAdded :: !Double
    , dribbleOppositionXGAdded :: !Double
    } deriving Show

awayFromOppositionDribbleOptions :: (Monad m, Match m, Log m) => Player -> m DribbleDesirability
awayFromOppositionDribbleOptions player = do
  (V2 nsX nsY) <- nearestSpace player
  let diff = playerPositionVector player - V3 nsX nsY 0
      ns = locate2D $ pure 2.5 * normalize diff
  curXG <- locationXG (playerTeam player) player
  newXG <- locationXG (playerTeam player) ns
  curOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  newOppXG <- locationXG (oppositionTeam $ playerTeam player) ns
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble player ball ns }
  trd <- interceptionTimePlayerBallRK False player ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  pure $ DribbleDesirability
    { dribbleTarget = DribbleAwayFromOpponents ns
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    }

towardsTouchlineDribbleOption :: (Monad m, Match m, Log m) => Player -> m DribbleDesirability
towardsTouchlineDribbleOption player = do
  attackingDirection' <- attackingDirection (playerTeam player)
  let dribbleLoc = 
        case attackingDirection' of
          AttackingLeftToRight -> playerPositionVector player + V3 2.5 0 0
          AttackingRightToLeft -> playerPositionVector player - V3 2.5 0 0
  curXG <- locationXG (playerTeam player) player
  newXG <- locationXG (playerTeam player) dribbleLoc
  curOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  newOppXG <- locationXG (oppositionTeam $ playerTeam player) dribbleLoc
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble player ball (locate2D dribbleLoc) }
  trd <- interceptionTimePlayerBallRK False player ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  pure $ DribbleDesirability
    { dribbleTarget = DribbleTowardsTouchline dribbleLoc
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    }

towardsGoalDribbleOption :: (Monad m, Match m, Log m) => Player -> m DribbleDesirability
towardsGoalDribbleOption player = do
  attackingDirection' <- attackingDirection (playerTeam player)
  pitch' <- pitch
  let finalDribbleLoc = 
        case attackingDirection' of
          AttackingLeftToRight ->   
            let (goalMin, goalMax) = rightGoalLine pitch'
            in (goalMin + goalMax)/2
          AttackingRightToLeft -> 
            let (goalMin, goalMax) = leftGoalLine pitch'
            in (goalMin + goalMax)/2
      dir = normalize (finalDribbleLoc - playerPositionVector player)
      dribbleLoc = playerPositionVector player + dir * 2.5
  curXG <- locationXG (playerTeam player) player
  newXG <- locationXG (playerTeam player) dribbleLoc
  curOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  newOppXG <- locationXG (oppositionTeam $ playerTeam player) dribbleLoc
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble player ball (locate2D dribbleLoc) }
  trd <- interceptionTimePlayerBallRK False player ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  pure $ DribbleDesirability
    { dribbleTarget = DribbleTowardsGoal dribbleLoc
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    }

desirableDribbleOptions :: (Monad m, Match m, Log m) => Player -> m [DribbleDesirability]
desirableDribbleOptions player = do
  ao <- awayFromOppositionDribbleOptions player
  tt <- towardsTouchlineDribbleOption player
  tg <- towardsGoalDribbleOption player
  pure [ao, tt, tg]

