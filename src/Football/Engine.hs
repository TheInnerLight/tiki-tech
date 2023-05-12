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
  , matchStatePlayer :: TVar Player
  }

class Engine m where
  kickBall :: (Double, Double) -> m ()
  canKick :: Player -> m Bool
  updateIntention :: Player -> m ()

kickImpl :: (Monad m, Has m MatchState, LiftSTM m) => (Double, Double) -> m ()
kickImpl (targetX, targetY) = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  liftSTM $ do
    ball <- readTVar stBall
    let (bx, by) = ballPosition ball
        ballDirection = normalize $ V3 (targetX - bx) (targetY - by) 0
        ball' = ball { ballMotionVector = ballDirection * 31 }
    writeTVar stBall ball'

updateIntentionImpl :: (Monad m, Has m MatchState, LiftSTM m, Engine m) => Player -> m ()
updateIntentionImpl player = do
    (state :: MatchState) <- has
    let stBall = matchStateBall state
    let stPlayer = matchStatePlayer state
    liftSTM $ do
      ball <- readTVar stBall
      let (px, py) = playerPosition player
          (bx, by) = ballPosition ball
          maxSpeed = playerSpeedMax $ playerSpeed player
          acceleration = playerSpeedAcceleration $ playerSpeed player
          direction = interceptionVector maxSpeed (bx, by) (ballMotionVector ball) (px, py) (playerMotionVector player)
          motion = playerMotionVector player
          motion' = maxMag (playerSpeedMax $ playerSpeed player) (motion + direction * pure acceleration)
          player' = player { playerMotionVector = motion' }
      writeTVar stPlayer player'
    ck <- canKick player
    when ck $ kickBall (55, 35)

canKickImpl :: (Monad m, Has m MatchState, LiftSTM m) => Player -> m Bool
canKickImpl player = do
    (state :: MatchState) <- has
    let stBall = matchStateBall state
    liftSTM $ do
      ball <- readTVar stBall
      let (px, py) = playerPosition player
          (bx, by) = ballPosition ball
          dist = sqrt ((px - bx) ** 2.0 + (py - by) ** 2.0)
      pure (dist < 0.3) 
  
