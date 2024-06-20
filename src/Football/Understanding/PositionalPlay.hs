{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Football.Understanding.PositionalPlay where

import Control.Lens ((^.))
import Football.Types
import Football.Match
import Core (Log (logOutput), Cache)
import Football.Understanding.Space.Data
import Linear (V2(..), R1 (_x), R2 (_y))
import Football.Understanding.Space (centresOfPlay, offsideLine, pitchHorizontalZone, getSpaceMapForTeam)
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Team (toTeamCoordinateSystem2D, fromTeamCoordinateSystem2D)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Data.Foldable (foldl')
import Data.Maybe (isJust, isNothing)
import Voronoi.JCVoronoi (JCVPoly(polyPoint, polyEdges, polyIndex), JCVEdge (jcvEdgePoint1, jcvEdgePoint2), voronoiPolygonArea)
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.Ord



allVerticalZones :: [VerticalZone]
allVerticalZones = [DefZone, DefMidZone, AttMidZone, AttZone]

allHorizontalZones :: [HorizontalZone]
allHorizontalZones = [WingHZ LeftHalf, HalfSpaceHZ LeftHalf, CentreHZ, HalfSpaceHZ RightHalf, WingHZ RightHalf]

allZones :: [(VerticalZone, HorizontalZone)]
allZones = liftM2 (,) allVerticalZones allHorizontalZones

zoneOccupation :: Map (VerticalZone, HorizontalZone) [PlayerState] -> (VerticalZone, HorizontalZone) -> (Int, Int)
zoneOccupation map' (vz, hz) =
  (foldl' f 0 allVerticalZones, foldl' f2 0 allHorizontalZones)
  where
    f c vz' =
      case Map.lookup (vz', hz) map' of
        Just _ -> c + 1
        Nothing -> c
    f2 c hz' =
      case Map.lookup (vz, hz') map' of
        Just _ -> c + 1
        Nothing -> c

verticalZone :: (Monad m, Match m, Locate2D x) => TeamId -> x -> m VerticalZone
verticalZone teamId x = do
  offsideLineX <- offsideLine teamId

  offsideXY <- toTeamCoordinateSystem2D teamId $ V2 offsideLineX 0
  posXY <- toTeamCoordinateSystem2D teamId $ locate2D x

  if posXY ^. _x > offsideXY ^. _x - 7.5 then
    pure AttZone
  else if posXY ^. _x > offsideXY ^. _x - 15 then
    pure AttMidZone
  else if posXY ^. _x > offsideXY ^. _x - 22.5 then
    pure DefMidZone
  else
    pure DefZone

verticalZonePos :: (Monad m, Match m) => TeamId -> VerticalZone -> m (Double, Double)
verticalZonePos teamId vz = do
  offsideLineX <- offsideLine teamId
  offsideXY <- toTeamCoordinateSystem2D teamId $ V2 offsideLineX 0
  ball <- gameBall
  ballXY <- toTeamCoordinateSystem2D teamId $ locate2D ball

  let offXY = V2 (min (ballXY ^. _x + 25) (offsideXY ^. _x)) 0

  let vzl AttZone = offXY - V2 0 0
      vzl AttMidZone = offXY - V2 7.5 0
      vzl DefMidZone = offXY - V2 15 0
      vzl DefZone = offXY - V2 22.5 0
  vzp <- fromTeamCoordinateSystem2D teamId $ vzl vz
  pure (vzp ^. _x, vzp ^. _x + 7.5)


horizontalZonePos :: (Monad m, Match m) => TeamId -> HorizontalZone -> m (Double, Double)
horizontalZonePos teamId hz = do
  let hzl (Just (WingHZ LeftHalf)) = V2 0 (-34)
      hzl (Just (HalfSpaceHZ LeftHalf)) = V2 0 (-14.66)
      hzl (Just CentreHZ) = V2 0 0
      hzl (Just (HalfSpaceHZ RightHalf)) = V2 0 14.66
      hzl (Just (WingHZ RightHalf)) = V2 0 27.08
      hzl Nothing = 34
      nextHz (WingHZ LeftHalf) = Just $ HalfSpaceHZ LeftHalf
      nextHz (HalfSpaceHZ LeftHalf) = Just CentreHZ
      nextHz CentreHZ = Just $ HalfSpaceHZ RightHalf
      nextHz (HalfSpaceHZ RightHalf) = Just $ WingHZ RightHalf
      nextHz (WingHZ RightHalf) = Nothing
  hzp <- fromTeamCoordinateSystem2D teamId . hzl $ Just hz
  hzp' <- fromTeamCoordinateSystem2D teamId . hzl $ nextHz hz
  pure (hzp ^. _y, hzp' ^. _y)

zonePos :: (Monad m, Match m) => TeamId -> (VerticalZone, HorizontalZone) -> m (V2 Double, V2 Double)
zonePos teamId (vz, hz) = do
  (vz0, vz1) <- verticalZonePos teamId vz
  (hz0, hz1) <- horizontalZonePos teamId hz
  pure (V2 vz0 hz0, V2 vz1 hz1)

data BlockOfSpace = BlockOfSpace
  { blockOfSpaceCentre :: V2 Double
  , blockOfSpaceArea :: Double
  } deriving Show

meanOn :: (Fractional a, Foldable t) => (x -> a) -> t x -> a
meanOn f polys =
  let (n, area) = foldl' (\(c, area') p -> (c+1, f p + area')) (0, 0) polys
  in (area/n)

findEdgeSpaces :: (Monad m, Match m, Cache m SpaceCache) => TeamId -> m [BlockOfSpace]
findEdgeSpaces team = do
  (SpaceMap spaceMap') <- getSpaceMapForTeam team
  let edgeMaker idx polyEdge =
        let (V2 ep1X ep1Y) = jcvEdgePoint1 polyEdge
            (V2 ep2X ep2Y) = jcvEdgePoint2 polyEdge
        in (V2 ep1X ep1Y, V2 ep2X ep2Y, Set.fromList [idx])
  let edgeFolder acc (e1, e2, sites) =
        case (Map.lookup e1 acc, Map.lookup e2 acc) of
          (Just v, Just v2)  -> Map.insert e1 (Set.union v sites) $ Map.insert e2 (Set.union v2 sites) acc
          (Just v, Nothing)  -> Map.insert e1 (Set.union v sites) $ Map.insert e2 sites acc
          (Nothing, Just v2) -> Map.insert e1 sites $ Map.insert e2 (Set.union v2 sites) acc
          (Nothing, Nothing) -> Map.insert e1 sites $ Map.insert e2 sites acc
  let spaceFolder poly acc =
        let edges = polyEdges $ spacePolyJCV poly
            idx = polyIndex $ spacePolyJCV poly
            nm = foldl' edgeFolder Map.empty (edgeMaker idx <$> edges)
        in Map.union nm acc
  let indexedEdgeSpaces = Map.foldr spaceFolder Map.empty spaceMap'
      mappedSpaces = Map.map (foldl' (\acc i -> case Map.lookup i spaceMap' of Just p -> p:acc; Nothing -> acc) [] . Set.toList) indexedEdgeSpaces
  pure $ (\(loc,polys) -> BlockOfSpace loc (meanOn (voronoiPolygonArea . spacePolyJCV) polys) ) <$> Map.toList mappedSpaces

zoneOptimalLocation :: (Monad m, Match m, Cache m SpaceCache, Log m) => TeamId -> (VerticalZone, HorizontalZone) -> m (V2 Double)
zoneOptimalLocation teamId (vz, hz) = do
  (start, end) <- zonePos teamId (vz, hz)

  opps <- oppositionPlayers teamId
  (SpaceMap spaceMap) <- getSpaceMapForTeam (oppositionTeam teamId)
  let spaceMapPolys = snd <$> Map.toList spaceMap

  edgeSpaces <- findEdgeSpaces (oppositionTeam teamId)

  let startX = min (start ^. _x) (end ^. _x)
      endX = max (start ^. _x) (end ^. _x)
      startY = min (start ^. _y) (end ^. _y)
      endY = max (start ^. _y) (end ^. _y)

  let polyThing sp =
        let pst = blockOfSpaceCentre sp
        in pst ^. _x >= startX && pst ^. _x <= endX && pst ^. _y >= startY && pst ^. _y <= endY

  let candidatePoints = L.sortOn (Data.Ord.Down . blockOfSpaceArea) (filter polyThing edgeSpaces)

  --logOutput candidatePoints

  case candidatePoints of
    [] -> pure $ (start + end) / 2
    p:_ -> pure $ blockOfSpaceCentre p



positionalPlayZone :: (Monad m, Match m, Locate2D x) => TeamId -> x -> m (VerticalZone, HorizontalZone)
positionalPlayZone teamId x = (,) <$> verticalZone teamId x <*> pitchHorizontalZone teamId x

ppInPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache, Cache m SpaceCache) => Player -> m (V2 Double)
ppInPossessionDesiredPosition player = do
  (V2 pCentreX pCentreY) <- centresOfPlayBothTeams <$> centresOfPlay
  ballXY <- locate2D <$> gameBall
  pitch' <- pitch
  offsideLineX <- offsideLine (playerTeamId player)
  attackingDirection' <- attackingDirection (playerTeamId player)

  playerSt <- getPlayerState player

  allTeamPlayers <- teamPlayers (playerTeamId player)

  zonesAndPlayers <- traverse (\p -> (, p) <$> positionalPlayZone (playerTeamId player) p )  allTeamPlayers
  let zonePlayers = Map.map (fmap snd) $ groupByKey fst zonesAndPlayers
  let validZonesToMoveTo = filter (\z -> isValid (zoneOccupation zonePlayers z) && isNothing (Map.lookup z zonePlayers)) allZones

  team <- getTeam (playerTeamId player)

  let (PositionalPlayInPossessionSystem ppMap) = teamInPossessionSystem team

  case Map.lookup player ppMap of
    Just (vz, hz) -> zoneOptimalLocation (playerTeamId player) (vz, hz)
    Nothing -> pure $ V2 0 0

  where
    isValid (vzCount, hzCount) = vzCount <= 3 && hzCount <= 2


groupByKey :: (Ord k) => (v -> k) -> [v] -> Map k [v]
groupByKey getkey
  = Map.fromListWith (++) . fmap (\val -> (getkey val, [val]))
