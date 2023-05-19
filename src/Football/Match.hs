{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Match where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Core
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad (when)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (spacePolyPlayer))
import qualified Data.Map as Map


class Match m where
  gameBall :: m Ball
  allPlayers :: m [Player]
  kickBall :: V3 Double -> m ()
  canKick :: Player -> m Bool
  update :: Int -> m ()
  spaceMap :: m SpaceMap
  
class HasTeam a where
  getTeam :: a -> Team

instance HasTeam Team where
  getTeam = id

instance HasTeam Player where
  getTeam = playerTeam

oppositionPlayers :: (Functor m, Match m) => Team -> m [Player]
oppositionPlayers team = filter (\p -> playerTeam p /= team) <$> allPlayers

teamPlayers :: (Functor m, Match m) => Team -> m [Player]
teamPlayers team = filter (\p -> playerTeam p == team) <$> allPlayers

teammates :: (Functor m, Match m) => Player -> m [Player]
teammates player = filter (\p -> playerNumber p /= playerNumber player) <$> teamPlayers (playerTeam player)

spaceMapForTeam :: (Functor m, Match m, HasTeam t) => t -> m [SpacePoly]
spaceMapForTeam t = mapper <$> spaceMap
  where
    mapper (SpaceMap m) = filter(\p -> getTeam (spacePolyPlayer p) ==  getTeam t ) $ snd <$> Map.toList m

spaceMapForOpposition :: (Functor m, Match m, HasTeam t) => t -> m [SpacePoly]
spaceMapForOpposition t = mapper <$> spaceMap
  where
    mapper (SpaceMap m) = filter(\p -> getTeam (spacePolyPlayer p) /=  getTeam t ) $ snd <$> Map.toList m


-- voronoiMapForTeam :: (Engine m) => Team -> m [JCVPoly]
-- voronoiMapForTeam Team1 = team1VoronoiMap
-- voronoiMapForTeam Team2 = team2VoronoiMap

-- voronoiMapForOpposition :: (Engine m) => Player -> m [JCVPoly]
-- voronoiMapForOpposition p | playerTeam p == Team1 = team2VoronoiMap
-- voronoiMapForOpposition p                         = team1VoronoiMap


  
