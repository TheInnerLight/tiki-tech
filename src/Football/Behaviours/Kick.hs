module Football.Behaviours.Kick where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance))
import Football.Ball
import Football.Player
import Football.Behaviours.Generic
import Data.List (sort)
import Football.Engine
import Control.Monad (when)

kickBallToLocation :: (Monad m, Engine m) => (Double, Double) -> Player -> m Player
kickBallToLocation location player = do
  player' <- runTowardsBall player
  ballInRange <- canKick player'
  when ballInRange $ kickBall location
  pure player'

