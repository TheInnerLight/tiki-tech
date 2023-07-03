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
  attackingDirection :: TeamId -> m AttackingDirection
  gameBall :: m Ball
  allPlayers :: m [PlayerState]
  getPlayerState :: Player -> m PlayerState
  getTeam :: TeamId -> m Team
  kickBall :: Player -> V3 Double -> V3 Double -> m Ball
  setBallMotionParams :: V3 Double -> V3 Double -> m Ball
  update :: Int -> m ()
  pitch :: m Pitch
  currentGameTime :: m GameTime
  matchEventLog :: m [MatchLogEntry]
  recordInMatchEventLog :: MatchLogEntry -> m ()
  getGameState :: m GameState
  setGameState :: GameState -> m ()

class HasTeamId a where
  getTeamId :: a -> TeamId

instance HasTeamId TeamId where
  getTeamId = id

instance HasTeamId Player where
  getTeamId = playerTeamId

oppositionPlayers :: (Functor m, Match m) => TeamId -> m [PlayerState]
oppositionPlayers team = filter (\p -> playerTeamId (playerStatePlayer p) /= team) <$> allPlayers

teamPlayers :: (Functor m, Match m) => TeamId -> m [PlayerState]
teamPlayers team = filter (\p -> playerTeamId (playerStatePlayer p) == team) <$> allPlayers

teammates :: (Functor m, Match m) => Player -> m [PlayerState]
teammates player = filter (\p -> playerStatePlayer p /= player) <$> teamPlayers (playerTeamId player)
 
clampPitch :: (Monad m, Match m) => V2 Double -> m (V2 Double)
clampPitch (V2 x y) = do
  pitch' <- pitch
  let x' = max (-pitchHalfLengthX pitch') $ min (pitchHalfLengthX pitch') x
  let y' = max (-pitchHalfWidthY pitch') $ min (pitchHalfWidthY pitch') y
  pure $ V2 x' y'
