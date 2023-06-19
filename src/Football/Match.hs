{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Match where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance, distance), normalize, V2 (V2))
import Football.Ball
import Core
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad (when)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (spacePolyPlayer))
import qualified Data.Map as Map
import Data.Foldable (find)
import Football.Locate2D (Locate2D(locate2D))
import Football.Types
import Data.List (groupBy, partition)
import Football.Pitch (pitchHalfLengthX, pitchHalfWidthY)

data AttackingDirection 
  = AttackingLeftToRight
  | AttackingRightToLeft

class Match m where
  attackingDirection :: Team -> m AttackingDirection
  gameBall :: m Ball
  allPlayers :: m [Player]
  kickBall :: Player -> V3 Double -> V3 Double -> m Ball
  setBallMotionParams :: V3 Double -> V3 Double -> m Ball
  update :: Int -> m ()
  pitch :: m Pitch
  currentGameTime :: m GameTime
  matchEventLog :: m [MatchLogEntry]
  recordInMatchEventLog :: MatchLogEntry -> m ()
  getGameState :: m GameState
  setGameState :: GameState -> m ()

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
 
clampPitch :: (Monad m, Match m) => V2 Double -> m (V2 Double)
clampPitch (V2 x y) = do
  pitch' <- pitch
  let x' = max (-pitchHalfLengthX pitch') $ min (pitchHalfLengthX pitch') x
  let y' = max (-pitchHalfWidthY pitch') $ min (pitchHalfWidthY pitch') y
  pure $ V2 x' y'
