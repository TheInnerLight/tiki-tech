module Football.Behaviours.Kick where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Football.Behaviours.Generic
import Data.List (sort)
import Football.Match
import Control.Monad (when)
import Core (GetSystemTime (systemTimeNow), Random (randomNormalMeanStd))
import Data.Time.Clock.System (SystemTime(..))

kickBallToLocation :: (Monad m, Match m, GetSystemTime m) => (Double, Double) -> Player -> m Player
kickBallToLocation location player = do
  player' <- runTowardsBall player
  ballInRange <- canKick player'
  if ballInRange then
    kickSuccess player'
  else 
    pure player'
  where
    kickSuccess player' = do
      ball <- gameBall
      time <- systemTimeNow
      kickBall $ motionVectorForPassTo ball location
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + 300000000 } }

controlBall :: (Monad m, Match m, GetSystemTime m, Random m) => Player -> m Player
controlBall player = do
  player' <- runTowardsBall player
  ballInRange <- canKick player'
  if ballInRange then
    kickSuccess player'
  else 
    pure player'
  where
    kickSuccess player' = do
      ball <- gameBall
      mult <- randomNormalMeanStd 1.0 0.2
      kickBall $ (- ballMotionVector ball + playerMotionVector player') * pure mult
      time <- systemTimeNow
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + 50000000 } }

motionVectorForPassTo :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassTo ball (targetX, targetY) = 
  ballDirection * pure (min 31 $ dist ** 0.35 * 4.5) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball - ballMotionVector ball)
    dist = norm (targetVector - ballPositionVector ball)
  

