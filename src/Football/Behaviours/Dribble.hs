{-# LANGUAGE FlexibleContexts #-}

module Football.Behaviours.Dribble where

import Linear (V3(..), normalize, V2 (V2))
import Football.Match
import Core (Log, Cache)
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
import Football.Understanding.Space.Data (SpaceCache)
import Football.Understanding.LineBreaking (linesBroken)
import Football.Understanding.Team (inTeamCoordinateSystem)

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
    , dribbleLinesBroken :: !Double
    } deriving Show

awayFromOppositionDribbleOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m DribbleDesirability
awayFromOppositionDribbleOptions player = do
  (V2 nsX nsY) <- nearestSpace player
  playerState <- getPlayerState player
  let diff = playerStatePositionVector playerState - V3 nsX nsY 0
      ns = locate2D $ pure 2.5 * normalize diff
  curXG <- locationXG (playerTeamId player) playerState
  newXG <- locationXG (playerTeamId player) ns
  curOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  newOppXG <- locationXG (oppositionTeam $ playerTeamId player) ns
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble playerState ball ns }
  trd <- interceptionTimePlayerBallRK False playerState ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  brokenLines <- linesBroken (playerTeamId player) (locate2D ball, ns)
  pure $ DribbleDesirability
    { dribbleTarget = DribbleAwayFromOpponents ns
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    , dribbleLinesBroken = brokenLines
    }

towardsTouchlineDribbleOption :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m DribbleDesirability
towardsTouchlineDribbleOption player = do
  playerState <- getPlayerState player
  dribbleLoc <- inTeamCoordinateSystem (playerTeamId player) (playerStatePositionVector playerState) (+ V3 2.5 0 0)
  curXG <- locationXG (playerTeamId player) playerState
  newXG <- locationXG (playerTeamId player) dribbleLoc
  curOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  newOppXG <- locationXG (oppositionTeam $ playerTeamId player) dribbleLoc
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble playerState ball (locate2D dribbleLoc) }
  trd <- interceptionTimePlayerBallRK False playerState ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  brokenLines <- linesBroken (playerTeamId player) (locate2D ball, locate2D dribbleLoc)
  pure $ DribbleDesirability
    { dribbleTarget = DribbleTowardsTouchline dribbleLoc
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    , dribbleLinesBroken = brokenLines
    }

towardsGoalDribbleOption :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m DribbleDesirability
towardsGoalDribbleOption player = do
  attackingDirection' <- attackingDirection (playerTeamId player)
  pitch' <- pitch
  playerState <- getPlayerState player
  let finalDribbleLoc = 
        case attackingDirection' of
          AttackingLeftToRight ->   
            let (goalMin, goalMax) = rightGoalLine pitch'
            in (goalMin + goalMax)/2
          AttackingRightToLeft -> 
            let (goalMin, goalMax) = leftGoalLine pitch'
            in (goalMin + goalMax)/2
      dir = normalize (finalDribbleLoc - playerStatePositionVector playerState)
      dribbleLoc = playerStatePositionVector playerState + dir * 2.5
  curXG <- locationXG (playerTeamId player) playerState
  newXG <- locationXG (playerTeamId player) dribbleLoc
  curOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  newOppXG <- locationXG (oppositionTeam $ playerTeamId player) dribbleLoc
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  ball <- gameBall
  let ball' = ball { ballMotionVector = motionVectorForDribble playerState ball (locate2D dribbleLoc) }
  trd <- interceptionTimePlayerBallRK False playerState ball'
  oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
  let z1 = (oid - trd) / sqrt 2
      a = 4.68
      b = 0.48
      safety = 1 / (1 + exp (-(a * z1 + b)))
  let xgAdded = newXG - curXG
  brokenLines <- linesBroken (playerTeamId player) (locate2D ball, locate2D dribbleLoc)
  pure $ DribbleDesirability
    { dribbleTarget = DribbleTowardsGoal dribbleLoc
    , dribbleDirection = ballMotionVector ball'
    , dribbleSafetyCoeff = safety
    , dribbleXGAdded = xgAdded
    , dribbleOppositionXGAdded = newOppXG - curOppXG
    , dribbleLinesBroken = brokenLines
    }

desirableDribbleOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [DribbleDesirability]
desirableDribbleOptions player = do
  ao <- awayFromOppositionDribbleOptions player
  tt <- towardsTouchlineDribbleOption player
  tg <- towardsGoalDribbleOption player
  pure [ao, tt, tg]

