{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Football.Behaviours.Marking where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm, distance))
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

playerMarkClosestOppositionPlayer :: (Monad m, Match m, Log m) => Player -> m (Maybe Player)
playerMarkClosestOppositionPlayer player = do
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  teamPlayers' <- teamPlayers (playerTeam player)
  let matchUp = foldl' (folder teamPlayers') Map.empty oppositionPlayers'
  pure $ Map.lookup player matchUp
  where 
    folder teamPlayers' acc p =
      case find (\p' -> isNothing $ Map.lookup p' acc) $ sortOn (distance (playerPositionVector p) . playerPositionVector) teamPlayers' of
        Just teamP -> Map.insert teamP p acc
        Nothing -> acc

positionalOrientedZonalMark :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (Double, Double)
positionalOrientedZonalMark player = do
  outOfPossessionDesiredPosition player

