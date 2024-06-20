module Football.Behaviours.Kick where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), normalize, project, V2 (V2))
import Football.Ball
import Football.Player
import Data.List (sort)
import Football.Match
import Control.Monad (when, void)
import Core (Random (randomNormalMeanStd, randomRange), Log (logOutput))
import Data.Time.Clock.System (SystemTime(..))
import Football.Types
import Football.Locate2D (Locate2D(locate2D))
import Football.Maths
import Football.GameTime (gameTimeAddSeconds)

canKick :: (Monad m, Match m, Log m) => PlayerState -> m (Maybe (V3 Double))
canKick player = do
  ball <- gameBall
  if distance (ballPositionVector ball) (playerStatePositionVector player) <= 0.5 then
    pure $ Just (ballPositionVector ball)
  else do
    let (dist, closestBallPos, _) = distanceAndClosestInterceptsWithinTimeStep (-1/30) (ballPositionVector ball, ballMotionVector ball) (playerStatePositionVector player, playerStateMotionVector player)

    if dist <= 0.7 then do
      logOutput $ "Can Kick!!!!" ++ show (playerStatePlayer player)
      pure $ Just closestBallPos
    else if dist <= 1.0 then do
      logOutput $ "Nearly!!!!" ++ show (playerStatePlayer player)
      pure Nothing
    else
      pure Nothing

kickBallInstr :: (Monad m, Match m, Log m) => PlayerState -> (V3 Double -> PlayerState -> m PlayerState) -> m PlayerState
kickBallInstr playerState kickSuccess = do
  ballInRange <- canKick playerState
  case ballInRange of
    Just r -> do
      playerState'' <-  kickSuccess r playerState
      let player = playerStatePlayer playerState''
      logOutput ("Player: " ++ show (playerTeamId player) ++ " " ++ show (playerNumber player) ++ " " ++ show (playerStateIntention playerState''))
      pure playerState''
    Nothing -> pure playerState

kickBallWithMotion :: (Monad m, Match m, Log m) => V3 Double -> TypeOfTouch -> PlayerState -> m PlayerState
kickBallWithMotion desiredBallMotion typeOfTouch playerState = do
  let f kickLoc playerState' = do
        time <- currentGameTime
        void $ kickBall (playerStatePlayer playerState') typeOfTouch kickLoc desiredBallMotion
        pure $ playerState' { playerStateIntention = IntentionCooldown $ gameTimeAddSeconds time 0.3 }
  kickBallInstr playerState f

dribbleToLocation :: (Monad m, Match m, Log m) =>  V3 Double -> PlayerState -> m PlayerState
dribbleToLocation  diff playerState = 
  let f kickLoc playerState' = do
        time <- currentGameTime
        ball' <- kickBall (playerStatePlayer playerState') DribbleTouch kickLoc diff
        let cooldownTime = 0.3
        let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
        pure $ playerState' { playerStateIntention = RunToLocation (locate2D eBallPos) $ gameTimeAddSeconds time cooldownTime }
  in kickBallInstr playerState f

controlBall :: (Monad m, Match m, Random m, Log m) => PlayerState -> m PlayerState
controlBall playerState = do
  let f kickLoc playerState' = do
        ball <- gameBall
        mult <- randomNormalMeanStd 1.0 0.05
        ball' <- kickBall (playerStatePlayer playerState') ControlTouch kickLoc $ (- ballMotionVector ball + playerStateMotionVector playerState' * 0.8) * pure mult
        time <- currentGameTime
        let cooldownTime = 0.1
        let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
        pure $ playerState' { playerStateIntention = IntentionCooldown $ gameTimeAddSeconds time cooldownTime }
  kickBallInstr playerState f

interceptBall :: (Monad m, Match m, Random m, Log m) => PlayerState -> m PlayerState
interceptBall playerState = do
  let f kickLoc playerState' = do
        ball <- gameBall
        mult <- randomNormalMeanStd 1.0 0.05
        ball' <- kickBall (playerStatePlayer playerState') InterceptionTouch kickLoc $ (- ballMotionVector ball + playerStateMotionVector playerState' * 0.8) * pure mult
        time <- currentGameTime
        let cooldownTime = 0.1
        let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
        pure $ playerState' { playerStateIntention = IntentionCooldown $ gameTimeAddSeconds time cooldownTime }
  kickBallInstr playerState f
  
tackle :: (Monad m, Match m, Random m, Log m) => PlayerState -> m PlayerState
tackle playerState = do
  let f kickLoc playerState' = do
        ball <- gameBall
        mult <- randomNormalMeanStd 1.0 0.05
        ball' <- kickBall (playerStatePlayer playerState') TackleTouch kickLoc $ (- ballMotionVector ball + playerStateMotionVector playerState' * 0.8) * pure mult
        time <- currentGameTime
        let cooldownTime = 0.1
        let eBallPos = ballPositionVector ball' + ballMotionVector ball' * pure cooldownTime
        pure $ playerState' { playerStateIntention = IntentionCooldown $ gameTimeAddSeconds time cooldownTime }
  kickBallInstr playerState f

motionVectorForDribble :: PlayerState -> Ball -> V2 Double -> V3 Double
motionVectorForDribble player ball (V2 targetX targetY) = 
  let ncv = V3 targetX targetY 0
      speedInDir = max 4.0 $ dot (normalize (ncv - ballPositionVector ball)) (playerStateMotionVector player)
  in maxMag (speedInDir+0.5) $ ncv - ballPositionVector ball -- - ballMotionVector ball

-- motionVectorForDribble :: Ball -> (Double, Double) -> V3 Double
-- motionVectorForDribble ball (targetX, targetY) = 
--   maxMag 31 $ ballDirection * pure (2.0 + 0.512 * dist - 4.27e-3 * dist ** 2.0 + 7.97e-5 * dist ** 3.0) - ballMotionVector ball
--   where
--     targetVector = V3 targetX targetY 0
--     ballDirection = normalize (targetVector - ballPositionVector ball)
--     dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToWeak :: Ball -> V2 Double -> V3 Double
motionVectorForPassToWeak ball (V2 targetX targetY) = 
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

motionVectorForPassTo :: Ball -> V2 Double -> V3 Double
motionVectorForPassTo ball (V2 targetX targetY) =
  maxMag 16 $ ballDirection * pure (10.9 * exp (0.0267 * dist)) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToMedium :: Ball -> V2 Double -> V3 Double
motionVectorForPassToMedium ball (V2 targetX targetY) = 
  maxMag 16 $ ballDirection * pure (8.18 * exp (0.0287 * dist)) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

timeForPassTo :: Ball -> V2 Double -> Double
timeForPassTo ball (V2 targetX targetY) = 
  0.655*exp(0.0345*dist)
  where
    targetVector = V3 targetX targetY 0
    dist = norm (targetVector - ballPositionVector ball)

motionVectorForPassToArrivalSpeed :: Double -> Ball -> V2 Double -> V3 Double
motionVectorForPassToArrivalSpeed arrivalSpeed ball (V2 targetX targetY) =
  maxMag 31 $ ballDirection * pure (arrivalSpeed + 0.15 + 0.434 * dist - 1.43e-3 * dist ** 2.0 + 5.9e-5 * dist ** 3.0) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

