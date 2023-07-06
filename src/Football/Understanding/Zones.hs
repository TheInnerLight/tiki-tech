{-# LANGUAGE FlexibleContexts #-}

module Football.Understanding.Zones where

import Football.Match
import Football.Understanding.Space.Data (SpacePoly (SpacePoly), CentresOfPlayCache)
import Voronoi.JCVoronoi (jcvSites2, JCVPoly (polyIndex))
import Football.Locate2D (Locate2D(locate2D))
import Football.Player (playerControlCentre, isGoalKeeper)
import qualified Data.Map as Map
import Data.Foldable (Foldable(foldl'))
import Football.Types
import Football.Understanding.Zones.Types (ZoneMap(..), ZoneCache)
import Core (cached, Cache, Log)
import Football.Understanding.Shape (outOfPossessionDesiredPosition)

createZonalMap :: (Match m, Monad m, Log m, Cache m CentresOfPlayCache) => TeamId -> m ZoneMap
createZonalMap team = do
  players' <- filter (not . isGoalKeeper . playerStatePlayer) <$> teamPlayers team
  let players'' = filter (\p -> case playerStateIntention p of WinBallIntention _ _ -> False; _ -> True) players'
  pos <- traverse (outOfPossessionDesiredPosition . playerStatePlayer) players''
  let allPlayersVoronoi = jcvSites2 pos
  let map1 = Map.fromList $ fmap (\v -> (polyIndex v, v)) allPlayersVoronoi
  let folder acc (i, p) =
        case Map.lookup i map1 of
          Just poly -> Map.insert p (SpacePoly poly p) acc
          Nothing -> acc
  let map2 = foldl' folder Map.empty $ zip [0..] (fmap playerStatePlayer players'')
  pure $ ZoneMap map2

zonePolyForPlayer :: Player -> ZoneMap -> Maybe SpacePoly
zonePolyForPlayer player (ZoneMap zp) = Map.lookup player zp

getZoneMap :: (Match m, Monad m, Log m, Cache m ZoneCache, Cache m CentresOfPlayCache ) => TeamId -> m ZoneMap
getZoneMap team = do 
  cached createZonalMap team

