{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Football.Behaviours.Marking where

import Control.Lens ((^.))
import Linear (V3(..), R1 (_x), R3 (_z), R2 (_y))
import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm, distance), V2)
import Data.List (sortOn, minimumBy, reverse, foldl', find)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV, spacePolyPlayer), SpaceMap (SpaceMap), CentresOfPlayCache)
import qualified Data.Map as Map
import Core (Log(..), Cache)
import Football.Behaviours.Kick (motionVectorForPassTo)
import Data.Maybe (isNothing)
import Football.Understanding.Space (centresOfPlay)
import Football.Understanding.Shape (outOfPossessionDesiredPosition)
import Football.Types
import Football.Behaviours.Marking.Zonal (mostDangerousPlayerInZone)
import Football.Understanding.Zones.Types (ZoneCache)
import Football.Understanding.Team (inTeamCoordinateSystem, toTeamCoordinateSystem, fromTeamCoordinateSystem)

playerMarkClosestOppositionPlayer :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
playerMarkClosestOppositionPlayer player = do
  oppositionPlayers' <- oppositionPlayers (playerTeamId player)
  teamPlayers' <- teamPlayers (playerTeamId player)
  ball <- gameBall
  playerState <- getPlayerState player
  let matchUp = foldl' (folder teamPlayers') Map.empty oppositionPlayers'
  case Map.lookup playerState matchUp of
    Just p | playerNumber player /= 1 -> do
      tBall <- toTeamCoordinateSystem (playerTeamId player) (ballPositionVector ball)
      tTargetPlayer <- toTeamCoordinateSystem (playerTeamId player) (playerStatePositionVector p)
      let tDiff = tBall - tTargetPlayer
      let desiredDist x = 1.8 * exp ( 0.075 * x )
      let desiredDistTP = desiredDist (norm tDiff)
      let minimumPos = tTargetPlayer - pure desiredDistTP * V3 1 0 0
      let maximumPos = tTargetPlayer - V3 1.8 0 0
      formationPos <- outOfPossessionDesiredPosition player
      let ultimatePos = resolvePosition minimumPos maximumPos formationPos
      locate2D <$> fromTeamCoordinateSystem (playerTeamId player) ultimatePos
      --locate2D <$> inTeamCoordinateSystem (playerTeam player) (playerPositionVector p) (+ V3 (-2) 0 0)
    _ -> outOfPossessionDesiredPosition player
  where 
    folder teamPlayers' acc p =
      case find (\p' -> isNothing $ Map.lookup p' acc) $ sortOn (distance (playerStatePositionVector p) . playerStatePositionVector) teamPlayers' of
        Just teamP -> Map.insert teamP p acc
        Nothing -> acc
    resolvePosition minPos maxPos formPos =
      let x = max (minPos ^. _x) $ min (maxPos ^. _x) (formPos ^. _x)
          y = maxPos ^. _y
      in V3 x y 0 



positionalOrientedZonalMark :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
positionalOrientedZonalMark player = do
  outOfPossessionDesiredPosition player

playerOrientedZonalMark :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache, Cache m ZoneCache) => Player -> m (V2 Double)
playerOrientedZonalMark player = do
  maybeMarkedPlayer <- mostDangerousPlayerInZone player
  case maybeMarkedPlayer of
    Just markedPlayer -> do
      markedPlayerState <- getPlayerState markedPlayer
      locate2D <$> inTeamCoordinateSystem (playerTeamId player) (playerStatePositionVector markedPlayerState) (+ V3 (-2) 0 0)
    Nothing           -> outOfPossessionDesiredPosition player

