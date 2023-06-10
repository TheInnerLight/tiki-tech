module Football.Behaviours.Pass where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm))
import Data.List (sortOn, minimumBy, reverse)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV, spacePolyPlayer), SpaceMap (SpaceMap))
import qualified Data.Map as Map
import Core (Log(..))
import Football.Behaviours.Kick (motionVectorForPassTo, motionVectorForPassToArrivalSpeed, timeForPassTo, motionVectorForPassToMedium)
import Football.Understanding.Space (offsideLine, isOnside)
import Control.Monad (filterM)
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Pitch (Pitch(Pitch))
import Football.Understanding.Interception (interceptionTimePlayerBallRK, interceptionTimePlayersBallRK)
import Football.Types

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
  } deriving (Eq, Show)

toFeetPassingOptions :: (Monad m, Match m, Log m) => Player -> m [PassDesirability]
toFeetPassingOptions player = do
  pitch' <- pitch
  teamPlayers' <- teammates player
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  let calcToFeetDesirability p1 = do
        let t = timeForPassTo ball $ locate2D p1 
            ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D (playerPositionVector p1 + playerMotionVector p1 * pure t) } 
            --trd = interceptionTimePlayersBallRK teamPlayers' ball'
        
        trd <- interceptionTimePlayerBallRK p1 ball'
        oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1
        pure $ PassDesirability 
          { passTarget = PlayerTarget p1
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = (newXG - originalXG)*safety
          , passOppositionXGAdded = (newOppXG - originalOppXG)*(1-safety)
          }
  onsidePlayers <- filterM (isOnside (playerTeam player)) teamPlayers'
  traverse calcToFeetDesirability onsidePlayers

throughBallPassingOptions :: (Monad m, Match m, Log m) => Player -> m [PassDesirability]
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
        trd <- interceptionTimePlayerBallRK p1 ball'
        oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1
        pure $ PassDesirability 
          { passTarget = AheadOfTarget p1
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = (newXG - originalXG)*safety
          , passOppositionXGAdded = (newOppXG - originalOppXG)*(1-safety)
          }
  onsidePlayers <- filterM (isOnside (playerTeam player)) $ filter (/= player) teamPlayers'
  let forwardRunningPlayers = filter (\p -> dot attackingRunVector (playerMotionVector p) > 4) onsidePlayers
  throughballDesirability <- traverse calcThroughBallDesirability forwardRunningPlayers
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) throughballDesirability

toSpacePassingOptions :: (Monad m, Match m, Log m) => Player -> m [PassDesirability]
toSpacePassingOptions player = do
  pitch' <- pitch
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  teamSpaceMap <- spaceMapForTeam player
  let calcToSpaceDesirability v1 = do
        let (centreX, centreY) = polyPoint $ spacePolyJCV v1
            ball' = ball { ballMotionVector = motionVectorForPassToMedium ball (centreX, centreY) }
        trd <- interceptionTimePlayerBallRK (spacePolyPlayer v1) ball'
        oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b)))
        newXG <- locationXG (playerTeam player) (centreX, centreY)
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) (centreX, centreY)
        pure $  PassDesirability 
          { passTarget = SpaceTarget (centreX, centreY)
          , passBallVector = ballMotionVector ball'
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = safety
          , passXGAdded = (newXG - originalXG)*safety
          , passOppositionXGAdded = (newOppXG - originalOppXG)*(1-safety)
          }
  onsidePolygons <- filterM (isOnside (playerTeam player) . spacePolyPlayer) $ filter (\p -> spacePolyPlayer p /= player) teamSpaceMap
  spaceDesirability <- traverse calcToSpaceDesirability onsidePolygons
  pure $ filter (\s -> passTeammateReceptionDistance s < 4.0) spaceDesirability

safestPassingOptions :: (Monad m, Match m, Log m) => Player -> m [PassDesirability]
safestPassingOptions player = do
  toFeetPassOptions <- toFeetPassingOptions player
  toSpacePassOptions <- toSpacePassingOptions player
  throughBallOptions <- throughBallPassingOptions player
  pure (toFeetPassOptions ++ toSpacePassOptions ++ throughBallOptions) 


