module Football.Events.Goal where

import Control.Lens ((^.))
import Football.Match
import Football.Types (Goal(..), Ball (ballPositionVector, ballMotionVector), Team (..))
import Linear (normalize, V3 (V3), _x, _y, _z)
import Football.Maths (linePlaneIntersection)
import Football.Pitch (Pitch(pitchWidth), leftGoalLine, rightGoalLine)
import Data.Maybe (fromJust)
import Core (Log(logOutput))

wasGoalScored :: (Match m, Monad m) => m (Maybe Goal)
wasGoalScored = do
  ball <- gameBall
  pitch' <- pitch
  let ballNow = ballPositionVector ball
  let ballPrev = ballPositionVector ball - ballMotionVector ball / 30
  let ballDir = normalize (ballNow - ballPrev)

  let (goal1Min, goal1Max) = leftGoalLine pitch'
  let goal1Pos = (goal1Min + goal1Max)/2
  let goal1Normal = V3 1 0 0
  let (goal2Min, goal2Max) = rightGoalLine pitch'
  let goal2Pos = (goal2Min + goal2Max)/2
  let goal2Normal = V3 (-1) 0 0
  
  lastTouch <- fromJust <$> lastTouchOfBall
  attackingDirection' <- attackingDirection Team1
  let (leftGoalScoredTeam, rightGoalScoredTeam)=
        case attackingDirection' of
          AttackingLeftToRight -> (Team2, Team1)
          AttackingRightToLeft -> (Team1, Team2)

  let intersecPoint1 = linePlaneIntersection (ballPrev, ballDir) (goal1Pos, goal1Normal)
  let intersecPoint2 = linePlaneIntersection (ballPrev, ballDir) (goal2Pos, goal2Normal)
  case (intersecPoint1, intersecPoint2) of
    (Just ip, _) | ballPrev ^. _x >= goal1Min ^. _x && ballNow  ^. _x < goal1Max ^. _x && ip ^. _y >= goal1Min ^. _y && ip ^. _y <= goal1Max ^. _y && ip ^. _z >= goal1Min ^. _z && ip ^. _z <= goal1Max ^. _z  -> pure $ Just $ Goal leftGoalScoredTeam lastTouch
    (_, Just ip) | ballPrev ^. _x <= goal2Min ^. _x && ballNow  ^. _x > goal2Max ^. _x && ip ^. _y >= goal2Min ^. _y && ip ^. _y <= goal2Max ^. _y && ip ^. _z >= goal2Min ^. _z && ip ^. _z <= goal2Max ^. _z  -> pure $ Just $ Goal rightGoalScoredTeam lastTouch
    _ -> pure Nothing
  
checkForGoal :: (Match m, Monad m, Log m) => m ()
checkForGoal = do
  maybeGoal <- wasGoalScored
  case maybeGoal of
    Just goal -> do
      recordGoal goal
      logOutput "------------------------------------------------------"
      logOutput "Goal!"
      logOutput goal
      logOutput "------------------------------------------------------"
    Nothing -> pure ()


