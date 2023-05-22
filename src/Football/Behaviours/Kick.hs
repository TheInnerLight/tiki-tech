module Football.Behaviours.Kick where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Data.List (sort)
import Football.Match
import Control.Monad (when, void)
import Core (GetSystemTime (systemTimeNow), Random (randomNormalMeanStd))
import Data.Time.Clock.System (SystemTime(..))

kickBallToLocation :: (Monad m, Match m, GetSystemTime m) => (Double, Double) -> (Double, Double) -> Player -> m Player
kickBallToLocation iceptLoc location player = do
  let player' = runTowardsLocation iceptLoc player
  ballInRange <- canKick player'
  if ballInRange then
    kickSuccess player'
  else 
    pure player'
  where
    kickSuccess player' = do
      ball <- gameBall
      time <- systemTimeNow
      void $ kickBall player $ motionVectorForPassTo ball location
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + 300000000 } }

controlBall :: (Monad m, Match m, GetSystemTime m, Random m) => (Double, Double) -> Player -> m Player
controlBall loc player = do
  let player' = runTowardsLocation loc player
  ballInRange <- canKick player'
  if ballInRange then
    kickSuccess player'
  else 
    pure player'
  where
    kickSuccess player' = do
      ball <- gameBall
      mult <- randomNormalMeanStd 1.0 0.2
      ball' <- kickBall player $ (- ballMotionVector ball + playerMotionVector player') * pure mult
      time <- systemTimeNow
      let cooldownTime = 0.1
      let cooldownTS = floor $ cooldownTime * 1e9
      let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + cooldownTS }, playerDesiredLocation = eBallPos }
  
motionVectorForPassTo :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassTo ball (targetX, targetY) = 
  maxMag 31 $ ballDirection * pure (2.71 + 0.512 * dist - 4.27e-3 * dist ** 2.0 + 7.97e-5 * dist ** 3.0) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

