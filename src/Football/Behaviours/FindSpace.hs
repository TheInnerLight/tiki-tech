{-# LANGUAGE TypeOperators #-}

module Football.Behaviours.FindSpace where



import Data.List.NonEmpty ( NonEmpty, fromList )

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance))
import Football.Ball
import Football.Player
import Data.List (sort, sortOn)
import Football.Match
import Core
import Voronoi.JCVoronoi
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..))
import qualified Data.Map as Map

optimalNearbySpace :: (Monad m, Match m) => Player -> m [V3 Double]
optimalNearbySpace player = do
  (SpaceMap spaceMap') <- spaceMap
  let polyDist poly =
        let (cx, cy) = polyPoint poly
        in distance (playerPositionVector player) (V3 cx cy 0)
      centreToVec :: JCVPoly -> V3 Double
      centreToVec poly =
        let (cx, cy) =  polyPoint poly
        in V3 cx cy 0
  pure $ take 1 
        $ fmap centreToVec
        $ reverse
        -- $ sortOn polyDist
        $ sortOn (\p ->  (voronoiPolygonArea p) / (polyDist p) ** 2.0 ) 
        $ spacePolyJCV . snd <$> Map.toList spaceMap'

