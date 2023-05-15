{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Engine where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Core
import Control.Concurrent.STM.TVar
import Control.Monad (when)

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar [Player]
  }

class Engine m where
  gameBall :: m Ball
  allPlayers :: m [Player]
  kickBall :: (Double, Double) -> m ()
  canKick :: Player -> m Bool
  update :: Int -> m ()

  
