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

data AttackingDirection 
  = AttackingLeftToRight
  | AttackingRightToLeft

class Match m where
  attackingDirection :: Team -> m AttackingDirection
  gameBall :: m Ball
  lastTouchOfBall :: m (Maybe Player)
  allPlayers :: m [Player]
  kickBall :: Player -> V3 Double -> m Ball
  update :: Int -> m ()
  spaceMap :: m SpaceMap
    
class HasTeam a where
  getTeam :: a -> Team

instance HasTeam Team where
  getTeam = id

instance HasTeam Player where
  getTeam = playerTeam

canKick :: (Monad m, Match m) => Player -> m Bool
canKick player = do
    ball <- gameBall
    let dist = norm (playerPositionVector player - ballPositionVector ball)
        ballMot' = pure (1.0/30.0) * ballMotionVector ball + pure (1.0/30.0) * playerMotionVector player
        intersec = (dot ballMot' (ballPositionVector ball - playerPositionVector player)) ** 2.0  - (quadrance (ballPositionVector ball - playerPositionVector player) - 0.5) ** 2.0
    pure $ intersec >= 0 || dist < 0.5

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

  
