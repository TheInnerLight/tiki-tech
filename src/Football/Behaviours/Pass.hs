{-# LANGUAGE FlexibleContexts #-}


module Football.Behaviours.Pass where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm), V2 (V2))
import Data.List (sortOn, minimumBy, reverse)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV, spacePolyPlayer), SpaceMap (SpaceMap), SpaceCache)
import qualified Data.Map as Map
import Core (Log(..), Cache)
import Football.Behaviours.Kick (motionVectorForPassTo, motionVectorForPassToArrivalSpeed, timeForPassTo, motionVectorForPassToMedium)
import Football.Understanding.Space (offsideLine, isOnside, getSpaceMapForTeam)
import Control.Monad (filterM)
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Pitch
import Football.Understanding.Interception (interceptionTimePlayerBallRK, interceptionTimePlayersBallRK)
import Football.Types
import Football.Understanding.LineBreaking (linesBroken)

data PassDesirability = PassDesirability
  { passTarget :: !PassTarget
  , passBallVector :: !(V3 Double)
  , passOppositionInterceptionDistance :: !Double
  , passTeammateReceptionDistance :: !Double
  -- | Approximate probability of successful pass completion
  , passSafetyCoeff :: !Double
  -- | Approximate XG added by the pass
  , passXGAdded :: !Double
  -- | Approximate opposition XG added by the pass (useful for judging e.g. how dangerous a backpass is)
  , passOppositionXGAdded :: !Double
  , passBrokenLines :: !Double
  } deriving (Eq, Show)

toFeetPassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
toFeetPassingOptions player = do
  teamPlayers' <- teammates player
  ball <- gameBall
  playerState <- getPlayerState player
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  originalXG <- locationXG (playerTeamId player) playerState
  originalOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  let calcToFeetDesirability p1State = do
        let t = timeForPassTo ball $ locate2D p1State 
            playerAtT = playerStatePositionVector p1State + playerStateMotionVector p1State * pure t
            ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D playerAtT } 
        trd <- interceptionTimePlayerBallRK False p1State ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeamId player) p1State
        newOppXG <- locationXG (oppositionTeam $ playerTeamId player) p1State
        brokenLines <- linesBroken (playerTeamId player) (locate2D ball, locate2D playerAtT)
        pure $ PassDesirability 
          { passTarget = PlayerTarget $ playerStatePlayer p1State
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = newXG - originalXG
          , passOppositionXGAdded = newOppXG - originalOppXG
          , passBrokenLines = brokenLines
          }
  onsidePlayers <- filterM (isOnside (playerTeamId player)) teamPlayers'
  traverse calcToFeetDesirability onsidePlayers

throughBallPassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
throughBallPassingOptions player = do
  teamPlayers' <- teammates player
  ball <- gameBall
  playerState <- getPlayerState player
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  originalXG <- locationXG (playerTeamId player) playerState
  originalOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  attackingDirection' <- attackingDirection (playerTeamId player)
  let attackingRunVector =
        case attackingDirection' of
          AttackingLeftToRight -> V3 1.0 0.0 0.0
          AttackingRightToLeft -> V3 (-1.0) 0.0 0.0
      calcThroughBallDesirability p1State = do
        let playerLocIn2S = playerStatePositionVector p1State + playerStateMotionVector p1State * 2
            ball' = ball { ballMotionVector = motionVectorForPassToArrivalSpeed (norm $ playerStateMotionVector p1State) ball $ locate2D playerLocIn2S }
            --trd = interceptionTimePlayersBallRK teamPlayers' ball'
        trd <- interceptionTimePlayerBallRK False p1State ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeamId player) p1State
        newOppXG <- locationXG (oppositionTeam $ playerTeamId player) p1State
        brokenLines <- linesBroken (playerTeamId player) (locate2D ball, locate2D playerLocIn2S)
        pure $ PassDesirability 
          { passTarget = AheadOfTarget $ playerStatePlayer p1State
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = newXG - originalXG
          , passOppositionXGAdded = newOppXG - originalOppXG
          , passBrokenLines = brokenLines
          }
  onsidePlayers <- filterM (isOnside (playerTeamId player)) $ filter (\p -> playerStatePlayer p /= player) teamPlayers'
  let forwardRunningPlayers = filter (\p -> dot attackingRunVector (playerStateMotionVector p) > 4) onsidePlayers
  throughballDesirability <- traverse calcThroughBallDesirability forwardRunningPlayers
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) throughballDesirability

toSpacePassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
toSpacePassingOptions player = do
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  playerState <- getPlayerState player
  originalXG <- locationXG (playerTeamId player) playerState
  originalOppXG <- locationXG (oppositionTeam $ playerTeamId player) playerState
  (SpaceMap teamSpaceMap) <- getSpaceMapForTeam (playerTeamId player)
  let calcToSpaceDesirability v1 = do
        let (V2 centreX centreY) = polyPoint $ spacePolyJCV v1
            ball' = ball { ballMotionVector = motionVectorForPassToMedium ball (V2 centreX centreY) }
        spacePolyPlayerState <- getPlayerState $ spacePolyPlayer v1
        trd <- interceptionTimePlayerBallRK False spacePolyPlayerState ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b)))
        newXG <- locationXG (playerTeamId player) (centreX, centreY)
        newOppXG <- locationXG (oppositionTeam $ playerTeamId player) (centreX, centreY)
        brokenLines <- linesBroken (playerTeamId player) (locate2D ball, V2 centreX centreY)
        pure $  PassDesirability 
          { passTarget = SpaceTarget (V2 centreX centreY)
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = newXG - originalXG
          , passOppositionXGAdded = newOppXG - originalOppXG
          , passBrokenLines = brokenLines
          }
      teamSpacePolys = snd <$> Map.toList teamSpaceMap
  onsidePolygons <- filterM checkOnside $ filter (\p -> spacePolyPlayer p /= player) teamSpacePolys
  spaceDesirability <- traverse calcToSpaceDesirability onsidePolygons
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) spaceDesirability
  where 
    checkOnside p = do
      isOnside (playerTeamId player) =<< getPlayerState (spacePolyPlayer p)

safestPassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
safestPassingOptions player = do
  toFeetPassOptions <- toFeetPassingOptions player
  toSpacePassOptions <- toSpacePassingOptions player
  throughBallOptions <- throughBallPassingOptions player
  pure (toFeetPassOptions ++ toSpacePassOptions ++ throughBallOptions) 


