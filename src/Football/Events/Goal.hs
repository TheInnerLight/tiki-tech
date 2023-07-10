module Football.Events.Goal where

import Control.Lens ((^.))
import Football.Match
import Football.Types
import Linear (normalize, V3 (V3), _x, _y, _z)
import Football.Maths (linePlaneIntersection)
import Football.Pitch (leftGoalLine, rightGoalLine)
import Data.Maybe (fromJust, listToMaybe)
import Core (Log(logOutput))
import Data.List (partition)
import Football.Events (touchEvents)

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
  time <- currentGameTime
  lte <- listToMaybe <$> touchEvents
  let ltp = touchOfBallPlayer <$> lte
  attackingDirection' <- attackingDirection TeamId1
  let (leftGoalScoredTeam, rightGoalScoredTeam)=
        case attackingDirection' of
          AttackingLeftToRight -> (TeamId2, TeamId1)
          AttackingRightToLeft -> (TeamId1, TeamId2)

  let intersecPoint1 = linePlaneIntersection (ballPrev, ballDir) (goal1Pos, goal1Normal)
  let intersecPoint2 = linePlaneIntersection (ballPrev, ballDir) (goal2Pos, goal2Normal)
  case (intersecPoint1, intersecPoint2, ltp) of
    (Just ip, _, Just lastTouch) | ballPrev ^. _x >= goal1Min ^. _x && ballNow  ^. _x < goal1Max ^. _x && ip ^. _y >= goal1Min ^. _y && ip ^. _y <= goal1Max ^. _y && ip ^. _z >= goal1Min ^. _z && ip ^. _z <= goal1Max ^. _z  -> pure $ Just $ Goal leftGoalScoredTeam lastTouch time
    (_, Just ip, Just lastTouch) | ballPrev ^. _x <= goal2Min ^. _x && ballNow  ^. _x > goal2Max ^. _x && ip ^. _y >= goal2Min ^. _y && ip ^. _y <= goal2Max ^. _y && ip ^. _z >= goal2Min ^. _z && ip ^. _z <= goal2Max ^. _z  -> pure $ Just $ Goal rightGoalScoredTeam lastTouch time
    _ -> pure Nothing
  
checkForGoal :: (Match m, Monad m, Log m) => m ()
checkForGoal = do
  maybeGoal <- wasGoalScored
  case maybeGoal of
    Just goal -> do
      recordInMatchEventLog $ GoalLogEntry goal
      logOutput "------------------------------------------------------"
      logOutput "Goal!"
      logOutput goal
      logOutput "------------------------------------------------------"
    Nothing -> pure ()

goals :: (Functor m, Match m) => m [Goal]
goals = 
  concatMap f <$> matchEventLog
  where
    f (GoalLogEntry goal) = [goal]
    f _                   = []

score :: (Functor m, Match m) => m (Int, Int)
score = (\(g1s, g2s) -> (length g1s, length g2s)) . partition (\ g -> goalTeam g == TeamId1) <$> goals

