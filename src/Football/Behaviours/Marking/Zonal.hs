{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Football.Behaviours.Marking.Zonal where

import Control.Lens ((^.))
import Football.Match 
import Core (Cache, Log)
import Football.Understanding.Zones.Types (ZoneCache)
import Football.Types
import Football.Understanding.Zones (getZoneMap, zonePolyForPlayer)
import Football.Understanding.Space.Data (CentresOfPlayCache, SpacePoly (spacePolyJCV))
import Voronoi.JCVoronoi (pointInPoly, JCVPoly (polyPoint))
import Football.Locate2D (Locate2D(locate2D))
import Data.Foldable (maximumBy)
import Football.Understanding.ExpectedGoals (locationXG)
import Data.Function (on)
import Football.Understanding.Space (mostAdvancedPlayer)
import Linear (V3(V3), R1 (_x))

maxByMaybe :: (a -> a -> Ordering) -> [a] -> Maybe a
maxByMaybe _ [] = Nothing
maxByMaybe f xs = Just $ maximumBy f xs

mostDangerousPlayerInZone :: (Match m, Monad m, Log m, Cache m ZoneCache, Cache m CentresOfPlayCache) => Player -> m (Maybe Player)
mostDangerousPlayerInZone player = do
  zmp <- getZoneMap (playerTeam player)
  opps <- oppositionPlayers (playerTeam player)
  let oppTeam = oppositionTeam (playerTeam player)
  let maybePoly = zonePolyForPlayer player zmp
  case maybePoly of
    Just poly -> do
      oppositionPlayersInPoly <- traverse (\p -> (p, ) <$> locationXG oppTeam p) $ filter (\op -> pointInPoly (locate2D op) (spacePolyJCV poly)) opps
      let maybeMostDangerous = maxByMaybe (compare `on` snd) oppositionPlayersInPoly
      pure $ playerStatePlayer . fst <$> maybeMostDangerous
    Nothing -> pure Nothing
  