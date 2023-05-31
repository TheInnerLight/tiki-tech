module Football.Behaviours.Dribble where

import Linear (V3(..))
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

data DribbleTarget 
  = DribbleAwayFromOpponents (Double, Double)
  | DribbleTowardsTouchline (V3 Double)
  | DribbleTowardsGoal (V3 Double)

data DribbleDesirability =
  DribbleDesirability
    { dribbleTarget :: !DribbleTarget
    , dribbleDirection :: !(V3 Double)
    , dribbleSafetyCoeff :: !Double
    , dribbleXGAdded :: !Double
    , dribbleDesirabilityCoeff :: !Double
    }

awayFromOppositionDribbleOptions :: (Monad m, Match m, Log m) => Player -> m DribbleDesirability
awayFromOppositionDribbleOptions player = do
  ns <- nearestSpace player
  curXG <- locationXG (playerTeam player) player
  newXG <- locationXG (playerTeam player) ns
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble player ball ns }
  trd <- interceptionTimePlayerBallRK player ball'
  oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
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
    , dribbleDesirabilityCoeff = xgAdded*safety  -- consider whether including opposition XG is helpful
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
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble player ball (locate2D dribbleLoc) }
  trd <- interceptionTimePlayerBallRK player ball'
  oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
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
    , dribbleDesirabilityCoeff = xgAdded*safety -- consider whether including opposition XG is helpful
    }

desirableDribbleOptions :: (Monad m, Match m, Log m) => Player -> m [DribbleDesirability]
desirableDribbleOptions player = do
  ao <- awayFromOppositionDribbleOptions player
  tt <- towardsTouchlineDribbleOption player
  pure $ sortOn (Data.Ord.Down . dribbleDesirabilityCoeff) [ao, tt]

