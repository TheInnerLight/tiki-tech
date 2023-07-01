{-# LANGUAGE FlexibleContexts #-}

module Football.Understanding.LineBreaking where

import Control.Lens ((^.))
import Football.Match
import Football.Understanding.Space.Data (SpaceMap (SpaceMap), SpaceCache, SpacePoly (spacePolyJCV, spacePolyPlayer))
import Linear (V2 (V2), Metric (dot, norm), R1 (_x), R2 (_y, _xy), normalize)
import Core (Cache, Log (logOutput))
import Football.Types 
import Football.Understanding.Space (getSpaceMapForTeam)
import Voronoi.JCVoronoi (JCVEdge(..), JCVPoly (polyEdges, polyIndex))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Data.List (foldl')
import Data.Maybe (isJust, maybeToList)
import Football.Maths (lineLineIntersection, linePlaneIntersection, linePlaneIntersection2D, lineSegmentPlaneIntersection2D)
import Football.Understanding.Pitch (targetGoalVector)
import Football.Locate2D (Locate2D(locate2D))
import Data.Foldable (foldlM)

oppositionLines :: (Monad m, Match m, Cache m SpaceCache) => Team -> m [(V2 Double, V2 Double)]
oppositionLines team = do
  (SpaceMap spaceMap') <- getSpaceMapForTeam $ oppositionTeam team
  ball <- gameBall
  tgVec <- targetGoalVector team
  let tgVec2D = tgVec ^. _xy
      tgVec2D' = tgVec2D - locate2D ball
      tgVecDir2D = normalize tgVec2D'
      tgVecPerp2D = V2 (- tgVecDir2D ^. _y) (tgVecDir2D ^. _x)

  let lineFolder acc (_, poly) = do
        let edges = polyEdges . spacePolyJCV $ poly
        playerState <- getPlayerState $ spacePolyPlayer poly
        let playerLoc = locate2D playerState
        let res = concatMap (\e -> maybeToList $ lineSegmentPlaneIntersection2D (jcvEdgePoint1 e, jcvEdgePoint2 e) (playerLoc, tgVecDir2D) ) edges

        pure $ case res of
          p1 : p2 : _ -> (p1, p2) : acc
            
  foldlM lineFolder [] $ Map.toList spaceMap'

linesBroken :: (Monad m, Match m, Cache m SpaceCache, Log m) => Team -> (V2 Double, V2 Double)  -> m Double
linesBroken team line = do
  oppLines <- oppositionLines team
  let (lv1, lv2) = line
      lv' = lv2 - lv1
      ld = normalize lv'

  tgVec <- targetGoalVector team
  let tgVec2D = tgVec ^. _xy
      tgVec2D' = tgVec2D - lv1
      tgVecDir2D = normalize tgVec2D'

  let crossedOppositionLines = length $  filter (\(lp1, lp2) -> isJust $ lineLineIntersection (lp1, lp2) (lv1, lv2)   )   oppLines
  let oppositionLinesToGoal = length $ filter (\(lp1, lp2) -> isJust $ lineLineIntersection (lp1, lp2) (lv1, tgVec2D)   )   oppLines 
  let adjustedOppositionLines = (ld `dot` tgVecDir2D) * fromIntegral crossedOppositionLines

  pure $ adjustedOppositionLines / fromIntegral oppositionLinesToGoal
