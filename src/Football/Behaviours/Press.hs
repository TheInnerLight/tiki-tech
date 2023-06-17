{-# LANGUAGE FlexibleContexts #-}

module Football.Behaviours.Press where

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
import Football.Understanding.Zones.Types (ZoneCache)
import Football.Types (Player (playerTeam, playerPositionVector), Ball (ballPositionVector))
import Football.Behaviours.Marking.Zonal (mostDangerousPlayerInZone)
import Football.Understanding.Shape (outOfPossessionDesiredPosition)

coverShadowOfPlayerOrientedZonalMark :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache, Cache m ZoneCache) => Player -> m (V2 Double)
coverShadowOfPlayerOrientedZonalMark player = do
  maybeMarkedPlayer <- mostDangerousPlayerInZone player
  ball <- gameBall
  let dir = normalize (ballPositionVector ball - playerPositionVector player)
  case maybeMarkedPlayer of
    Just markedPlayer -> pure $ locate2D $ playerPositionVector markedPlayer + dir * pure 2
    Nothing -> outOfPossessionDesiredPosition player

