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

canKick :: (Monad m, Match m, Log m) => Player -> m (Maybe (V3 Double))
canKick player = do
  ball <- gameBall
  if distance (ballPositionVector ball) (playerPositionVector player) <= 0.5 then
    pure $ Just (ballPositionVector ball)
  else do
    let ballPos = ballPositionVector ball
        playerPos = playerPositionVector player
        posDiff = ballPos - playerPos
        ballDir =  - normalize (ballMotionVector ball)
        normalToBallDir = normalize $ V3 (ballDir ^. _x) (-ballDir ^. _y) 0
        playerDir = - normalize (playerMotionVector player)
        normalToPlayerDir = normalize $ V3 (playerDir ^. _x) (-playerDir ^. _y) 0
        crossNormal = normalize $ cross normalToBallDir normalToPlayerDir
        rejection = posDiff - project posDiff normalToPlayerDir - project posDiff crossNormal
        sDistanceToLinePos = norm rejection / dot normalToBallDir (normalize rejection)
    if abs sDistanceToLinePos < 0.5 then
      pure $ Just $ ballPos - normalToBallDir * pure sDistanceToLinePos
    else
      pure Nothing
        --closestApproach = ballPos - normalToBallDir * pure sDistanceToLinePos
    
    -- let (p0x, p0y) = locate2D $ playerPositionVector player - playerMotionVector player / 30
    --     (p1x, p1y) = locate2D $ playerPositionVector player
    --     (b0x, b0y) = locate2D $ ballPositionVector ball - ballMotionVector ball / 30
    --     (b1x, b1y) = locate2D $ ballMotionVector ball

    --     denom = (p0x - p1x) * (b0y - b1y) - (p0y - p1y) * (b0x - b1x)
    --     td = (p0x - b0x) * (b0y - b1y) - (p0y - b0y) * (b0x - b1x)
    --     ud = (p0x - b0x) * (p0y - p1y) - (p0y - p0y) * (p0x - p1x)
    --     t = td / denom
    --     u = ud / denom

    -- if t >= 0 && t <= 1 && u >= 0 && u <= 1 then
    --   pure $ Just $ V3 (p0x + t * (p1x-p0x)) (p0y + t * (p1y-p0y)) 0
    -- else
    --   pure Nothing

        --if (denom /= 0)


    -- let relBallMot = -ballMotionVector ball
    --     relBallDir = normalize relBallMot
    --     relPlayerMot = -playerMotionVector player
    --     relPlayerDir = normalize relPlayerMot
    --     normalToBallDir = normalize $ V3 (relBallDir ^. _x) (-relBallDir ^. _y) 0
    --     normalToPlayerDir = normalize $ V3 (relPlayerDir ^. _x) (-relPlayerDir ^. _y) 0
    --     perpBallDist = dot normalToBallDir (playerPositionVector player - ballPositionVector ball)
    --     perpPlayerDist = dot normalToPlayerDir (playerPositionVector player - ballPositionVector ball)

    -- if perpBallDist <= 0.5 then do
    --   let parDist = dot relBallDir (playerPositionVector player - ballPositionVector ball)
    --       closestPoint = ballPositionVector ball + pure parDist * relBallDir
    --   if distance closestPoint (playerPositionVector player) <=  0.5 && parDist >= 0  && parDist <= norm relBallMot / 30 then
    --     pure $ Just closestPoint
    --   else
    --     pure Nothing
    -- else 
    --   pure Nothing

kickBallWith :: (Monad m, Match m, GetSystemTime m, Log m) => (Double, Double) -> V3 Double -> Player -> m Player
kickBallWith iceptLoc desiredBallMotion player = do
  let player' = runTowardsLocation iceptLoc player
  ballInRange <- canKick player'
  case ballInRange of
    Just r -> do
      player'' <-  kickSuccess r player'
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player'
  where
    kickSuccess kickLoc player' = do
      time <- systemTimeNow
      void $ kickBall player kickLoc desiredBallMotion
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + 300000000 } }

dribbleToLocation :: (Monad m, Match m, GetSystemTime m, Log m) => (Double, Double) -> V3 Double -> Player -> m Player
dribbleToLocation iceptLoc diff player = do
  let player' = runTowardsLocation iceptLoc player
  ballInRange <- canKick player'
  case ballInRange of
    Just r -> do
      player'' <-  kickSuccess r player'
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player'
  where
    kickSuccess kickLoc player' = do
      time <- systemTimeNow
      ball' <- kickBall player kickLoc diff
      let cooldownTime = 0.3
      let cooldownTS = floor $ cooldownTime * 1e9
      let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + cooldownTS }, playerDesiredLocation = eBallPos }

controlBall :: (Monad m, Match m, GetSystemTime m, Random m, Log m) => (Double, Double) -> Player -> m Player
controlBall loc player = do
  let player' = runTowardsLocation loc player
  ballInRange <- canKick player'
  case ballInRange of
    Just r -> do 
      player'' <- kickSuccess r player'
      logOutput ("Player: " ++ show (playerTeam player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerIntention player))
      pure player''
    Nothing -> pure player'
  where
    kickSuccess kickLoc player' = do
      ball <- gameBall
      mult <- randomNormalMeanStd 1.0 0.15
      ball' <- kickBall player kickLoc $ (- ballMotionVector ball + playerMotionVector player') * pure mult
      time <- systemTimeNow
      let cooldownTime = 0.1
      let cooldownTS = floor $ cooldownTime * 1e9
      let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
      pure $ player' { playerIntention = IntentionCooldown $ time { systemNanoseconds = systemNanoseconds time + cooldownTS }, playerDesiredLocation = eBallPos }

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
  maxMag 31 $ ballDirection * pure (10.9 * exp (0.027 * dist)) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

timeForPassTo :: Ball -> (Double, Double) -> Double
timeForPassTo ball (targetX, targetY) = 
  0.951*exp(0.0351*dist)
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

