{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Match where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance, distance), normalize)
import Football.Ball
import Football.Player
import Core
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad (when)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (spacePolyPlayer))
import qualified Data.Map as Map
import Football.Pitch (Pitch)
import Data.Foldable (find)
import Football.Locate2D (Locate2D(locate2D))
import Football.Types
import Data.List (groupBy, partition)

data AttackingDirection 
  = AttackingLeftToRight
  | AttackingRightToLeft

class Match m where
  attackingDirection :: Team -> m AttackingDirection
  gameBall :: m Ball
  lastTouchOfBall :: m (Maybe Player)
  allPlayers :: m [Player]
  kickBall :: Player -> V3 Double -> V3 Double -> m Ball
  update :: Int -> m ()
  spaceMap :: m SpaceMap
  pitch :: m Pitch
  goals :: m [Goal]
  recordGoal :: Goal -> m ()
  currentGameTime :: m GameTime

class HasTeam a where
  getTeam :: a -> Team

instance HasTeam Team where
  getTeam = id

instance HasTeam Player where
  getTeam = playerTeam

score :: (Functor m, Match m) => m (Int, Int)
score = (\(g1s, g2s) -> (length g1s, length g2s)) . partition (\ g -> goalTeam g == Team1) <$> goals

oppositionPlayers :: (Functor m, Match m) => Team -> m [Player]
oppositionPlayers team = filter (\p -> playerTeam p /= team) <$> allPlayers

teamPlayers :: (Functor m, Match m) => Team -> m [Player]
teamPlayers team = filter (\p -> playerTeam p == team) <$> allPlayers

teammates :: (Functor m, Match m) => Player -> m [Player]
teammates player = filter (\p -> playerNumber p /= playerNumber player) <$> teamPlayers (playerTeam player)

spaceMapForTeam :: (Functor m, Match m, HasTeam t) => t -> m [SpacePoly]
spaceMapForTeam t = mapper <$> spaceMap
  where
    mapper (SpaceMap m) = filter(\p -> getTeam (spacePolyPlayer p) == getTeam t ) $ snd <$> Map.toList m

spaceMapForOpposition :: (Functor m, Match m, HasTeam t) => t -> m [SpacePoly]
spaceMapForOpposition t = mapper <$> spaceMap
  where
    mapper (SpaceMap m) = filter(\p -> getTeam (spacePolyPlayer p) /= getTeam t ) $ snd <$> Map.toList m


clampPitch :: (Applicative m, Match m) => (Double, Double) -> m (Double, Double)
clampPitch (x, y) = pure (max 0 $ min 105 x, max 0 $ min 68 y)

  
