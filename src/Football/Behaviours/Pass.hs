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
  pitch' <- pitch
  teamPlayers' <- teammates player
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  let calcToFeetDesirability p1 = do
        let t = timeForPassTo ball $ locate2D p1 
            playerAtT = playerPositionVector p1 + playerMotionVector p1 * pure t
            ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D playerAtT } 
            --trd = interceptionTimePlayersBallRK teamPlayers' ball'
        trd <- interceptionTimePlayerBallRK False p1 ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1
        brokenLines <- linesBroken (playerTeam player) (locate2D ball, locate2D playerAtT)
        pure $ PassDesirability 
          { passTarget = PlayerTarget p1
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = newXG - originalXG
          , passOppositionXGAdded = newOppXG - originalOppXG
          , passBrokenLines = brokenLines
          }
  onsidePlayers <- filterM (isOnside (playerTeam player)) teamPlayers'
  traverse calcToFeetDesirability onsidePlayers

throughBallPassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
throughBallPassingOptions player = do
  pitch' <- pitch
  teamPlayers' <- teammates player
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  attackingDirection' <- attackingDirection (playerTeam player)
  let attackingRunVector =
        case attackingDirection' of
          AttackingLeftToRight -> V3 1.0 0.0 0.0
          AttackingRightToLeft -> V3 (-1.0) 0.0 0.0
      calcThroughBallDesirability p1 = do
        let playerLocIn2S = playerPositionVector p1 + playerMotionVector p1 * 2
            ball' = ball { ballMotionVector = motionVectorForPassToArrivalSpeed (norm $ playerMotionVector p1) ball $ locate2D playerLocIn2S }
            --trd = interceptionTimePlayersBallRK teamPlayers' ball'
        trd <- interceptionTimePlayerBallRK False p1 ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1
        brokenLines <- linesBroken (playerTeam player) (locate2D ball, locate2D playerLocIn2S)
        pure $ PassDesirability 
          { passTarget = AheadOfTarget p1
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = newXG - originalXG
          , passOppositionXGAdded = newOppXG - originalOppXG
          , passBrokenLines = brokenLines
          }
  onsidePlayers <- filterM (isOnside (playerTeam player)) $ filter (/= player) teamPlayers'
  let forwardRunningPlayers = filter (\p -> dot attackingRunVector (playerMotionVector p) > 4) onsidePlayers
  throughballDesirability <- traverse calcThroughBallDesirability forwardRunningPlayers
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) throughballDesirability

toSpacePassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
toSpacePassingOptions player = do
  pitch' <- pitch
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  (SpaceMap teamSpaceMap) <- getSpaceMapForTeam (playerTeam player)
  let calcToSpaceDesirability v1 = do
        let (V2 centreX centreY) = polyPoint $ spacePolyJCV v1
            ball' = ball { ballMotionVector = motionVectorForPassToMedium ball (V2 centreX centreY) }
        trd <- interceptionTimePlayerBallRK False (spacePolyPlayer v1) ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b)))
        newXG <- locationXG (playerTeam player) (centreX, centreY)
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) (centreX, centreY)
        brokenLines <- linesBroken (playerTeam player) (locate2D ball, V2 centreX centreY)
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
  onsidePolygons <- filterM (isOnside (playerTeam player) . spacePolyPlayer) $ filter (\p -> spacePolyPlayer p /= player) teamSpacePolys
  spaceDesirability <- traverse calcToSpaceDesirability onsidePolygons
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) spaceDesirability

safestPassingOptions :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m [PassDesirability]
safestPassingOptions player = do
  toFeetPassOptions <- toFeetPassingOptions player
  toSpacePassOptions <- toSpacePassingOptions player
  throughBallOptions <- throughBallPassingOptions player
  pure (toFeetPassOptions ++ toSpacePassOptions ++ throughBallOptions) 


