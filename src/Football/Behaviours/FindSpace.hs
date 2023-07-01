{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Football.Behaviours.FindSpace where



import Data.List.NonEmpty ( NonEmpty, fromList )

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), V2 (V2))
import Football.Ball
import Football.Player
import Data.List (foldl', sort, sortOn)

import Football.Match
import Football.Types
import Core
import Voronoi.JCVoronoi
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..), HorizontalZone (HalfSpaceHZ, WingHZ, CentreHZ), HorizontalHalf (LeftHalf, RightHalf), CentresOfPlayCache, SpaceCache)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Football.Understanding.Space (offsideLine, centresOfPlay, pitchHorizontalZone, getSpaceMapForTeam, getSpaceMap)
import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import Football.Understanding.Shape (inPossessionDesiredPosition)
import Data.Foldable (foldrM,find)
import qualified Data.Ord


data EdgeInd = EdgeInd Int Int
  deriving (Eq, Ord, Show)

edge :: Int -> Int -> EdgeInd
edge i j =
  EdgeInd (min i j) (max i j)

data BlockOfSpace = BlockOfSpace
  { blockOfSpaceCentre :: V2 Double
  , blockOfSpaceArea :: Double
  } deriving Show

meanOn :: (Fractional a, Foldable t) => (x -> a) -> t x -> a
meanOn f polys =
  let (n, area) = foldl' (\(c, area') p -> (c+1, f p + area')) (0, 0) polys
  in (area/n)

voronoiPolygonAdjustedArea :: Team -> SpacePoly -> Double
voronoiPolygonAdjustedArea team poly  =
  let spacePoly = spacePolyJCV poly
      polyPlayer = spacePolyPlayer poly
  in if playerTeam polyPlayer == team then
    voronoiPolygonArea spacePoly
  else
    voronoiPolygonArea spacePoly / 4

findEdgeSpaces :: (Monad m, Match m, Cache m SpaceCache) => Team -> m [BlockOfSpace]
findEdgeSpaces team = do
  (SpaceMap spaceMap') <- getSpaceMapForTeam team
  let edgeMaker idx polyEdge =
        let (V2 ep1X ep1Y) = jcvEdgePoint1 polyEdge
            (V2 ep2X ep2Y) = jcvEdgePoint2 polyEdge
        in (V2 ep1X ep1Y, V2 ep2X ep2Y, Set.fromList [idx])
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
  pure $ (\(loc,polys) -> BlockOfSpace loc (meanOn (voronoiPolygonAdjustedArea team) polys) ) <$> Map.toList mappedSpaces


findPolySpaces :: (Monad m, Match m, Cache m SpaceCache) => Player -> m [BlockOfSpace]
findPolySpaces player = do
  (SpaceMap spaceMap') <- getSpaceMap
  let polyToBlock poly =
        BlockOfSpace (polyPoint $ spacePolyJCV poly) (voronoiPolygonArea $ spacePolyJCV poly)
  let spaceMap'' = Map.filter (\p -> playerTeam (spacePolyPlayer p) /= playerTeam player ) spaceMap'
  pure $ (\(_, poly) -> polyToBlock poly) <$> Map.toList spaceMap''
  

optimalNearbySpace :: (Monad m, Match m, Log m, Cache m SpaceCache, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
optimalNearbySpace player = do
    polySpaces <- findPolySpaces player
    polyEdges' <- findEdgeSpaces (oppositionTeam $ playerTeam player)

    let allSpaces  = polyEdges' ++ polySpaces
    let filterPitchArea p = do
          let (V2 x y) = blockOfSpaceCentre p
          (V2 desiredX desiredY) <- inPossessionDesiredPosition player
          let r = sqrt((desiredX-x)**2.0 + (desiredY-y)**2.0)
          pure $ r <= 15

    shapePosition <- inPossessionDesiredPosition player
    teammates' <- teammates player
    let teammatePlayers = playerStatePlayer <$> teammates'

    let buildAssignments p (assignments, spaces) = do
          filtered <- filterM filterPitchArea spaces
          desiredPos <- inPossessionDesiredPosition player
          let r v = distance v desiredPos
          let sorted = sortOn (Data.Ord.Down . (\p' -> blockOfSpaceArea p' / r (blockOfSpaceCentre p') ** 1.5)) filtered
          pure $ case listToMaybe $ blockOfSpaceCentre <$> sorted of
            Just c -> (Map.insert p c assignments, tail sorted)
            Nothing -> (Map.insert p shapePosition assignments, sorted)

    assignments <- fst <$> foldrM buildAssignments (Map.empty, allSpaces) (player : teammatePlayers)
    pure $ assignments ! player

nearestSpace :: (Monad m, Match m, Log m, Cache m SpaceCache) => Player -> m (V2 Double)
nearestSpace player = do
  (SpaceMap spaceMap') <- getSpaceMap
  case find (\p -> spacePolyPlayer p == player) spaceMap' of
    Just p  -> pure $ polyPoint $ spacePolyJCV p
    Nothing -> error "Supplied player did not have an assigned voronoi polygon (this should never occur!)"

findClosestOpposition :: (Monad m, Match m) => Player ->  m Player
findClosestOpposition player = do
  opp <- oppositionPlayers (playerTeam player)
  playerState <- getPlayerState player
  pure $ playerStatePlayer $ head $ sortOn (\o -> distance (playerStatePositionVector o) (playerStatePositionVector playerState) ) opp
  
