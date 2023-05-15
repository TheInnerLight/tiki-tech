{-# LANGUAGE TypeOperators #-}

module Football.Behaviours.FindSpace where

import Data.Ext ( type (:+)(..) )
import Data.Geometry ( Point(Point2) )  
import Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer
    ( delaunayTriangulation )
import Data.List.NonEmpty ( NonEmpty, fromList )
import Algorithms.Geometry.DelaunayTriangulation.Types (toPlanarSubdivision, toPlaneGraph)

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance))
import Football.Ball
import Football.Player
import Football.Behaviours.Generic
import Data.List (sort, sortOn)
import Football.Engine
import Voronoi.Fortune
import Core

findSpace :: (Monad m, Engine m, Log m) => Player -> m Player
findSpace player = do
  players <- allPlayers
  let teamPlayers = filter (\p -> playerTeam p /= playerTeam player) players

  
  let points = filter (\(x,y) -> x >= 0 && x <= 105 && y >=0 && y <= 68) $ fmap playerPositionPoint teamPlayers
  let polygons = voronoiPolygons ((0,0),(105,68)) points

      closestCentre = if length points > 2 then
        let polygons = voronoiPolygons ((0,0),(105,68)) points
            centres = fmap (\(x,y) -> V3 x y 0) $ fmap voronoiPolygonCentre polygons
            polyDist poly =
              let (cx, cy) =  voronoiPolygonCentre poly
              in distance (playerPositionVector player) (V3 cx cy 0)
            centreToVec :: VoronoiPolygon -> V3 Double
            centreToVec poly =
              let (cx, cy) =  voronoiPolygonCentre poly
              in V3 cx cy 0
        in 
          take 1 
            $ filter (\p -> p ^. _x >= 0 && p ^. _x <= 105 && p ^. _y >=0 && p ^. _y <= 68)
            $ fmap centreToVec
            $ reverse
            $ sortOn (\p ->  (voronoiPolygonArea p) ) 
            $ polygons
      else []

  case closestCentre of
    c : _ -> do 
      logOutput polygons
      runTowardsLocation (c ^. _x, c ^. _y) player
    _ -> stop player
  where 
    playerPositionPoint p = 
      let ppv = playerPositionVector p
      in (ppv ^. _x, ppv ^. _y) 


