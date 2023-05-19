module Football.Behaviours.Generic where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3))

runTowardsBall :: (Monad m, Match m) => Player -> m Player
runTowardsBall player = do
  ball <- gameBall
  let ppv = playerPositionVector player
      bpv = ballPositionVector ball
      maxSpeed = playerSpeedMax $ playerSpeed player
      acceleration = playerSpeedAcceleration $ playerSpeed player
      direction = interceptionVector maxSpeed bpv (ballMotionVector ball) ppv (playerMotionVector player)
      motion = playerMotionVector player
      motion' = maxMag maxSpeed (motion + direction * pure acceleration)
      player' = player { playerMotionVector = motion' }
  pure player'

runTowardsLocation :: (Monad m, Match m) => (Double, Double) -> Player -> m Player
runTowardsLocation (x, y) player = do
  let ppv = playerPositionVector player
      targetV = V3 x y 0
      maxSpeed = playerSpeedMax $ playerSpeed player
      acceleration = playerSpeedAcceleration $ playerSpeed player
      direction = normalize (targetV - ppv)
      motion = playerMotionVector player
      motion' = maxMag maxSpeed (motion + direction * pure acceleration)
      player' = player { playerMotionVector = motion' }
  pure player'

stop :: (Monad m, Match m) => Player -> m Player
stop player = do
  ball <- gameBall
  let maxSpeed = playerSpeedMax $ playerSpeed player
      acceleration = playerSpeedAcceleration $ playerSpeed player
      direction =  - (normalize $ playerMotionVector player)
      motion = playerMotionVector player
      motion' = maxMag maxSpeed (motion + direction * pure acceleration)
      player' = player { playerMotionVector = motion' }
  pure player'

