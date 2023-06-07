module Football.Behaviours.Kick where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), normalize, project)
import Football.Ball
import Football.Player
import Data.List (sort)
import Football.Match
import Control.Monad (when, void)
import Core (GetSystemTime (systemTimeNow), Random (randomNormalMeanStd), Log (logOutput))
import Data.Time.Clock.System (SystemTime(..))
import Football.Types
import Football.Locate2D (Locate2D(locate2D))
import Football.Maths

canKick :: (Monad m, Match m, Log m) => Player -> m (Maybe (V3 Double))
canKick player = do
  ball <- gameBall
  if distance (ballPositionVector ball) (playerPositionVector player) <= 0.5 then
    pure $ Just (ballPositionVector ball)
  else do
    let (dist, closestBallPos, _) = distanceAndClosestInterceptsWithinTimeStep (-1/30) (ballPositionVector ball, ballMotionVector ball) (playerPositionVector player, playerMotionVector player)
    if dist <= 0.7 then do
      pure $ Just closestBallPos
    else
      pure Nothing

kickBallWith :: (Monad m, Match m, GetSystemTime m, Log m) => (Double, Double) -> V3 Double -> Player -> m Player
kickBallWith iceptLoc desiredBallMotion player = do
  ballInRange <- canKick player
  case ballInRange of
    Just r -> do
      player'' <-  kickSuccess r player
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player
  where
    kickSuccess kickLoc player' = do
      time <- systemTimeNow
      void $ kickBall player kickLoc desiredBallMotion
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + 300000000 } }

dribbleToLocation :: (Monad m, Match m, GetSystemTime m, Log m) => (Double, Double) -> V3 Double -> Player -> m Player
dribbleToLocation iceptLoc diff player = do
  ballInRange <- canKick player
  case ballInRange of
    Just r -> do
      player'' <-  kickSuccess r player
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player
  where
    kickSuccess kickLoc player' = do
      time <- systemTimeNow
      ball' <- kickBall player' kickLoc diff
      let cooldownTime = 0.3
      let cooldownTS = floor $ cooldownTime * 1e9
      let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
      pure $ player' { playerIntention = RunToLocation (locate2D eBallPos) $ time { systemNanoseconds = systemNanoseconds time + cooldownTS } }

controlBall :: (Monad m, Match m, GetSystemTime m, Random m, Log m) => (Double, Double) -> Player -> m Player
controlBall loc player = do
  ballInRange <- canKick player
  case ballInRange of
    Just r -> do 
      player'' <- kickSuccess r player
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player
  where
    kickSuccess kickLoc player' = do
      ball <- gameBall
      mult <- randomNormalMeanStd 1.0 0.1
      ball' <- kickBall player kickLoc $ (- ballMotionVector ball + playerMotionVector player') * pure mult
      time <- systemTimeNow
      let cooldownTime = 0.1
      let cooldownTS = floor $ cooldownTime * 1e9
      let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + cooldownTS } }

motionVectorForDribble :: Player -> Ball -> (Double, Double) -> V3 Double
motionVectorForDribble player ball (targetX, targetY) = 
  let ncv = V3 targetX targetY 0
      speedInDir = max 4.0 $ dot (normalize (ncv - ballPositionVector ball)) (playerMotionVector player)
  in maxMag (speedInDir+0.2) $ ncv - ballPositionVector ball -- - ballMotionVector ball

-- motionVectorForDribble :: Ball -> (Double, Double) -> V3 Double
-- motionVectorForDribble ball (targetX, targetY) = 
--   maxMag 31 $ ballDirection * pure (2.0 + 0.512 * dist - 4.27e-3 * dist ** 2.0 + 7.97e-5 * dist ** 3.0) - ballMotionVector ball
--   where
--     targetVector = V3 targetX targetY 0
--     ballDirection = normalize (targetVector - ballPositionVector ball)
--     dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToWeak :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassToWeak ball (targetX, targetY) = 
  maxMag 31 $ ballDirection * pure (2.71 + 0.512 * dist - 4.27e-3 * dist ** 2.0 + 7.97e-5 * dist ** 3.0) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

-- motionVectorForPassTo :: Ball -> (Double, Double) -> V3 Double
-- motionVectorForPassTo ball (targetX, targetY) = 
--   maxMag 31 $ ballDirection * pure (10.15 + 0.434 * dist - 1.43e-3 * dist ** 2.0 + 5.9e-5 * dist ** 3.0) - ballMotionVector ball
--   where
--     targetVector = V3 targetX targetY 0
--     ballDirection = normalize (targetVector - ballPositionVector ball)
--     dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassTo :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassTo ball (targetX, targetY) = 
  maxMag 31 $ ballDirection * pure (10.9 * exp (0.0267 * dist)) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToMedium :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassToMedium ball (targetX, targetY) = 
  maxMag 31 $ ballDirection * pure (8.18 * exp (0.0287 * dist)) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

timeForPassTo :: Ball -> (Double, Double) -> Double
timeForPassTo ball (targetX, targetY) = 
  0.655*exp(0.0345*dist)
  where
    targetVector = V3 targetX targetY 0
    dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToArrivalSpeed :: Double -> Ball -> (Double, Double) -> V3 Double
motionVectorForPassToArrivalSpeed arrivalSpeed ball (targetX, targetY) = 
  maxMag 31 $ ballDirection * pure (arrivalSpeed + 0.15 + 0.434 * dist - 1.43e-3 * dist ** 2.0 + 5.9e-5 * dist ** 3.0) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

