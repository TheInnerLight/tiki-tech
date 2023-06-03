{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Football.Behaviours.FindSpace where



import Data.List.NonEmpty ( NonEmpty, fromList )

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance))
import Football.Ball
import Football.Player
import Data.List (foldl', sort, sortOn)

import Football.Match
import Football.Types
import Core
import Voronoi.JCVoronoi
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..), HorizontalZone (HalfSpaceHZ, WingHZ, CentreHZ), HorizontalHalf (LeftHalf, RightHalf), CentresOfPlayCache)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Football.Understanding.Space (offsideLine, centresOfPlay, pitchHorizontalZone)
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Football.Understanding.Shape (inPossessionDesiredPosition)
import Data.Foldable (foldrM,find)


data EdgeInd = EdgeInd Int Int
  deriving (Eq, Ord, Show)

edge :: Int -> Int -> EdgeInd
edge i j =
  EdgeInd (min i j) (max i j)

data BlockOfSpace = BlockOfSpace
  { blockOfSpaceCentre :: (Double, Double)
  , blockOfSpaceArea :: Double
  } deriving Show

meanOn :: (Fractional a, Foldable t) => (x -> a) -> t x -> a
meanOn f polys =
  let (n, area) = foldl' (\(c, area) p -> (c+1, f p + area)) (0, 0) polys
  in (area/n)


findEdgeSpaces :: (Monad m, Match m) => m [BlockOfSpace]
findEdgeSpaces = do
  (SpaceMap spaceMap') <- spaceMap
  let edgeMaker idx polyEdge =
        let (ep1X, ep1Y) = jcvEdgePoint1 polyEdge
            (ep2X, ep2Y) = jcvEdgePoint2 polyEdge
        in ((ep1X, ep1Y), (ep2X, ep2Y), Set.fromList [idx])
  let edgeFolder acc (e1, e2, sites) =
        case (Map.lookup e1 acc, Map.lookup e2 acc) of
          (Just v, Just v2) -> Map.insert e1 (Set.union v sites) $ Map.insert e2 (Set.union v2 sites) acc
          (Just v, Nothing) -> Map.insert e1 (Set.union v sites) $ Map.insert e2 sites acc
          (Nothing, Just v2) -> Map.insert e1 sites $ Map.insert e2 (Set.union v2 sites) acc
          (Nothing, Nothing) -> Map.insert e1 sites $ Map.insert e2 sites acc
  let spaceFolder poly acc = 
        let edges = polyEdges $ spacePolyJCV poly
            idx = polyIndex $ spacePolyJCV poly
            nm = foldl' edgeFolder Map.empty (edgeMaker idx <$> edges)
        in Map.union nm acc
  let indexedEdgeSpaces = Map.foldr spaceFolder Map.empty spaceMap'
      mappedSpaces = Map.map (foldl' (\acc i -> case Map.lookup i spaceMap' of Just p -> p:acc; Nothing -> acc) [] . Set.toList) indexedEdgeSpaces
  pure $ fmap (\(loc,polys) -> BlockOfSpace loc (meanOn (voronoiPolygonArea . spacePolyJCV) polys) ) $ Map.toList mappedSpaces


findPolySpaces :: (Monad m, Match m) => Player -> m [BlockOfSpace]
findPolySpaces player = do
  (SpaceMap spaceMap') <- spaceMap
  let polyToBlock poly =
        BlockOfSpace (polyPoint $ spacePolyJCV poly) (voronoiPolygonArea $ spacePolyJCV poly)
  let spaceMap'' = Map.filter (\p -> playerTeam (spacePolyPlayer p) /= playerTeam player ) spaceMap'
  pure $ (\(_, poly) -> polyToBlock poly) <$> Map.toList spaceMap''
  

optimalNearbySpace :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (Double, Double)
optimalNearbySpace player = do
    polySpaces <- findPolySpaces player
    polyEdges' <- findEdgeSpaces
    let allSpaces  = polySpaces ++ polyEdges'

    let filterPitchArea p = do
          let (x, y) = blockOfSpaceCentre p
          (desiredX, desiredY) <- inPossessionDesiredPosition player
          let r = sqrt((desiredX-x)**2.0 + (desiredY-y)**2.0)
          pure $ r <= 10

    shapePosition <- inPossessionDesiredPosition player

    teammates' <- teammates player

    let buildAssignments p (assignments, spaces) = do
          filtered <- filterM filterPitchArea spaces
          (desiredX, desiredY) <- inPossessionDesiredPosition player
          let r (x, y) = sqrt((desiredX-x)**2.0 + (desiredY-y)**2.0)
          let sorted = reverse $ sortOn (\p -> (blockOfSpaceArea p) / (r $ blockOfSpaceCentre p) ** 1.5) filtered
          pure $ case (listToMaybe $ blockOfSpaceCentre <$> sorted) of
            Just c -> (Map.insert p c assignments, tail sorted)
            Nothing -> (Map.insert p shapePosition assignments, sorted)

    assignments <- fst <$> foldrM buildAssignments (Map.empty, allSpaces) (player : teammates')
    pure $ assignments ! player

nearestSpace :: (Monad m, Match m, Log m) => Player -> m (Double, Double)
nearestSpace player = do
  (SpaceMap spaceMap') <- spaceMap
  case (find (\p -> spacePolyPlayer p == player) spaceMap') of
    Just p -> pure $ polyPoint $ spacePolyJCV p

findClosestOpposition :: (Monad m, Match m) => Player ->  m Player
findClosestOpposition player = do
  opp <- oppositionPlayers (playerTeam player)
  pure $ head $ sortOn (\o -> distance (playerPositionVector o) (playerPositionVector player) ) opp
  
