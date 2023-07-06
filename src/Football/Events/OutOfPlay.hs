module Football.Events.OutOfPlay where

import Control.Lens ((^.))
import Football.Match
import Football.Types
import Linear (normalize, V3 (V3), _x, _y, _z, V2 (V2), Metric (dot))
import Football.Maths (linePlaneIntersection)
import Football.Pitch (leftGoalLine, rightGoalLine, pitchHalfLengthX, pitchHalfWidthY)
import Data.Maybe (fromJust, listToMaybe)
import Core (Log(logOutput))
import Data.List (partition)
import Football.Events (touchEvents)
import Football.Locate2D (Locate2D(locate2D))

wentForThrowIn :: (Match m, Monad m) => m (Maybe (Team, V2 Double))
wentForThrowIn = do
  ball <- gameBall
  pitch' <- pitch
  let ballNow = ballPositionVector ball
  let ballPrev = ballPositionVector ball - 0.1 * ballMotionVector ball
  let ballDir = normalize (ballPrev - ballNow)

  let (leftTouchLineMin, leftTouchLineMax) = (V3 (-pitchHalfLengthX pitch') (-pitchHalfWidthY pitch') 0, V3 (pitchHalfLengthX pitch') (-pitchHalfWidthY pitch') 0)
  let leftTouchLineNormal = V3 0 1 0
  let leftTouchLineCentre = V3 0 (-pitchHalfWidthY pitch') 0
  let (rightTouchLineMin, rightTouchLineMax) = (V3 (-pitchHalfLengthX pitch') (pitchHalfWidthY pitch') 0, V3 (pitchHalfLengthX pitch') (pitchHalfWidthY pitch') 0)
  let rightTouchLineNormal = V3 0 (-1) 0
  let rightTouchLineCentre = V3 0 (pitchHalfWidthY pitch') 0

  time <- currentGameTime
  lte <- listToMaybe <$> touchEvents
  let ltp = touchOfBallPlayer <$> lte

  gs <- getGameState

  let intersecPoint1 = linePlaneIntersection (ballNow, ballDir) (leftTouchLineCentre, leftTouchLineNormal)
  let intersecPoint2 = linePlaneIntersection (ballNow, ballDir) (rightTouchLineCentre, rightTouchLineNormal)
  case (intersecPoint1, intersecPoint2, ltp, gs) of
    (Just ip, _, Just lastTouch, OpenPlay) | ballNow  ^. _y < leftTouchLineMax ^. _y && ip ^. _x >= leftTouchLineMin ^. _x && ip ^. _x <= leftTouchLineMax ^. _x && leftTouchLineNormal `dot` ballDir > 0  -> 
      let lastTouchTeam = playerTeam lastTouch
          touchLineLoc = locate2D ball
      in pure $ Just (oppositionTeam lastTouchTeam, touchLineLoc)
    (_, Just ip, Just lastTouch, OpenPlay) | ballNow  ^. _y > rightTouchLineMax ^. _y && ip ^. _x >= rightTouchLineMin ^. _x && ip ^. _x <= rightTouchLineMax ^. _x && rightTouchLineNormal `dot` ballDir > 0 -> 
      let lastTouchTeam = playerTeam lastTouch
          touchLineLoc = locate2D ball
      in pure $ Just (oppositionTeam lastTouchTeam, touchLineLoc)
    _ -> pure Nothing
  
checkForThrowIn :: (Match m, Monad m, Log m) => m ()
checkForThrowIn = do
  maybeThrow <- wentForThrowIn
  case maybeThrow of
    Just (team, loc) -> do
      _ <- setBallMotionParams (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
      setGameState $ ThrowIn team loc
    Nothing -> pure ()

data CrossedGoalLine 
  = CrossedForCorner (V2 Double) Team
  | CrossedForGoalKick (V2 Double) Team
  | CrossedForGoal Goal

crossedGoalLine :: (Match m, Monad m) => m (Maybe CrossedGoalLine)
crossedGoalLine = do
  ball <- gameBall
  pitch' <- pitch
  let ballNow = ballPositionVector ball
  let ballPrev = ballPositionVector ball - 0.1 * ballMotionVector ball
  let ballDir = normalize (ballPrev - ballNow)

  let (goal1Min, goal1Max) = leftGoalLine pitch'
  let goal1Pos = (goal1Min + goal1Max)/2
  let goal1Normal = V3 1 0 0
  let (goal2Min, goal2Max) = rightGoalLine pitch'
  let goal2Pos = (goal2Min + goal2Max)/2
  let goal2Normal = V3 (-1) 0 0
  time <- currentGameTime
  lte <- listToMaybe <$> touchEvents
  let ltp = touchOfBallPlayer <$> lte
  attackingDirection' <- attackingDirection Team1
  let (leftGoalScoredTeam, rightGoalScoredTeam)=
        case attackingDirection' of
          AttackingLeftToRight -> (Team2, Team1)
          AttackingRightToLeft -> (Team1, Team2)

  let intersecPoint1 = linePlaneIntersection (ballNow, ballDir) (goal1Pos, goal1Normal)
  let intersecPoint2 = linePlaneIntersection (ballNow, ballDir) (goal2Pos, goal2Normal)

  gs <- getGameState

  case (intersecPoint1, intersecPoint2, ltp, gs) of
    (Just ip, _, Just lastTouch, OpenPlay) | ballNow  ^. _x < goal1Max ^. _x && ip ^. _y < goal1Min ^. _y -> 
      if playerTeam lastTouch == leftGoalScoredTeam then pure $ Just $ CrossedForGoalKick (V2 (-pitchHalfLengthX pitch' + 5) (goal1Pos ^. _y)) rightGoalScoredTeam else pure $ Just $ CrossedForCorner (V2 (-pitchHalfLengthX pitch') (-pitchHalfWidthY pitch')) leftGoalScoredTeam
    (Just ip, _, Just lastTouch, OpenPlay) | ballNow  ^. _x < goal1Max ^. _x && ip ^. _y > goal1Max ^. _y -> 
      if playerTeam lastTouch == leftGoalScoredTeam then pure $ Just $ CrossedForGoalKick (V2 (-pitchHalfLengthX pitch' + 5) (goal1Pos ^. _y)) rightGoalScoredTeam else pure $ Just $ CrossedForCorner (V2 (-pitchHalfLengthX pitch') (pitchHalfWidthY pitch')) leftGoalScoredTeam
    (_, Just ip, Just lastTouch, OpenPlay) | ballNow  ^. _x > goal2Max ^. _x && ip ^. _y < goal2Min ^. _y -> 
      if playerTeam lastTouch == rightGoalScoredTeam then pure $ Just $ CrossedForGoalKick (V2 (pitchHalfLengthX pitch' - 5) (goal2Pos ^. _y)) leftGoalScoredTeam else pure $ Just $ CrossedForCorner (V2 (pitchHalfLengthX pitch') (-pitchHalfWidthY pitch')) rightGoalScoredTeam
    (_, Just ip, Just lastTouch, OpenPlay) | ballNow  ^. _x > goal2Max ^. _x && ip ^. _y > goal2Max ^. _y -> 
      if playerTeam lastTouch == rightGoalScoredTeam then pure $ Just $ CrossedForGoalKick (V2 (pitchHalfLengthX pitch' - 5) (goal2Pos ^. _y)) leftGoalScoredTeam else pure $ Just $ CrossedForCorner (V2 (pitchHalfLengthX pitch') (pitchHalfWidthY pitch')) rightGoalScoredTeam
    (Just ip, _, Just lastTouch, OpenPlay) | ballNow  ^. _x < goal1Max ^. _x && ip ^. _y >= goal1Min ^. _y && ip ^. _y <= goal1Max ^. _y && ip ^. _z >= goal1Min ^. _z && ip ^. _z <= goal1Max ^. _z  -> 
      pure $ Just $ CrossedForGoal $ Goal leftGoalScoredTeam lastTouch time
    (_, Just ip, Just lastTouch, OpenPlay) | ballNow  ^. _x > goal2Max ^. _x && ip ^. _y >= goal2Min ^. _y && ip ^. _y <= goal2Max ^. _y && ip ^. _z >= goal2Min ^. _z && ip ^. _z <= goal2Max ^. _z  -> 
      pure $ Just $ CrossedForGoal $ Goal rightGoalScoredTeam lastTouch time
    _ -> pure Nothing
  
checkedCrossedGoalLine :: (Match m, Monad m, Log m) => m ()
checkedCrossedGoalLine = do
  maybeCrossedLine <- crossedGoalLine
  pitch' <- pitch
  case maybeCrossedLine of
    Just (CrossedForCorner loc team) -> do
      _ <- setBallMotionParams (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
      setGameState $ CornerKick team loc
    Just (CrossedForGoalKick loc team) -> do
      _ <- setBallMotionParams (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
      setGameState $ GoalKick team loc
    Just (CrossedForGoal goal) -> do
      _ <- setBallMotionParams (V3 0 0 0) (V3 0 0 0)
      recordInMatchEventLog $ GoalLogEntry goal
      logOutput "------------------------------------------------------"
      logOutput "Goal!"
      logOutput goal
      logOutput "------------------------------------------------------"
      setGameState $ KickOff $ oppositionTeam $ goalTeam goal
    Nothing -> pure ()
