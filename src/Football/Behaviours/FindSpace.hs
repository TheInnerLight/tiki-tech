{-# LANGUAGE TypeOperators #-}

module Football.Behaviours.FindSpace where



import Data.List.NonEmpty ( NonEmpty, fromList )

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance))
import Football.Ball
import Football.Player
import Football.Behaviours.Generic
import Data.List (sort, sortOn)
import Football.Engine
--import Voronoi.Fortune
import Core
import Voronoi.JCVoronoi

optimalNearbySpace :: Player -> [Player] -> [V3 Double]
optimalNearbySpace player oppositionPlayers = 
  let points = filter (\(x,y) -> x >= 0 && x <= 105 && y >=0 && y <= 68) $ fmap playerPositionPoint oppositionPlayers
      polygons' = jcvSites2 points
  in if length points > 2 then
    let -- polygons = voronoiPolygons ((0,0),(105,68)) points
        --centres = fmap (\(x,y) -> V3 x y 0) $ fmap polyPoint polygons'
        polyDist poly =
          let (cx, cy) =  polyPoint poly
          in distance (playerPositionVector player) (V3 cx cy 0)
        centreToVec :: JCVPoly -> V3 Double
        centreToVec poly =
          let (cx, cy) =  polyPoint poly
          in V3 cx cy 0
    in 
      take 1 
        $ fmap centreToVec
        $ reverse
        $ sortOn (\p ->  (voronoiPolygonArea p) / sqrt (polyDist p) ) 
        $ polygons'
    else []
  where 
    playerPositionPoint p = 
      let ppv = playerPositionVector p
      in (ppv ^. _x, ppv ^. _y) 



findSpace :: (Monad m, Engine m, Log m) => Player -> m Player
findSpace player = do
  players <- allPlayers
  let oppositionPlayers = filter (\p -> playerTeam p /= playerTeam player) players
  let closestCentre = optimalNearbySpace player oppositionPlayers
  case closestCentre of
    c : _ -> do 
      --logOutput polygons
      runTowardsLocation (c ^. _x, c ^. _y) player
    _ -> stop player
  where 
    playerPositionPoint p = 
      let ppv = playerPositionVector p
      in (ppv ^. _x, ppv ^. _y) 


