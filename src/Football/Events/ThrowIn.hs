module Football.Events.ThrowIn where

import Control.Lens ((^.))
import Football.Match
import Football.Types
import Linear (normalize, V3 (V3), _x, _y, _z)
import Football.Maths (linePlaneIntersection)
import Football.Pitch (Pitch(pitchWidth, pitchLength), leftGoalLine, rightGoalLine)
import Data.Maybe (fromJust, listToMaybe)
import Core (Log(logOutput))
import Data.List (partition)
import Football.Events (touchEvents)
import Football.Locate2D (Locate2D(locate2D))

wentForThrowIn :: (Match m, Monad m) => m (Maybe (Team, (Double, Double)))
wentForThrowIn = do
  ball <- gameBall
  pitch' <- pitch
  let ballNow = ballPositionVector ball
  let ballPrev = ballPositionVector ball - ballMotionVector ball / 30
  let ballDir = normalize (ballNow - ballPrev)

  let (leftTouchLineMin, leftTouchLineMax) = (V3 0 0 0, V3 (pitchLength pitch') 0 0)
  let leftTouchLineNormal = V3 0 1 0
  let leftTouchLineCentre = V3 (pitchLength pitch' / 2) 0 0
  let (rightTouchLineMin, rightTouchLineMax) = (V3 0 (pitchWidth pitch') 0, V3 (pitchLength pitch') (pitchWidth pitch') 0)
  let rightTouchLineNormal = V3 0 (-1) 0
  let rightTouchLineCentre = V3 (pitchLength pitch' / 2) (pitchWidth pitch') 0

  time <- currentGameTime
  lte <- listToMaybe <$> touchEvents
  let ltp = touchOfBallPlayer <$> lte

  let intersecPoint1 = linePlaneIntersection (ballPrev, ballDir) (leftTouchLineCentre, leftTouchLineNormal)
  let intersecPoint2 = linePlaneIntersection (ballPrev, ballDir) (rightTouchLineCentre, rightTouchLineNormal)
  case (intersecPoint1, intersecPoint2, ltp) of
    (Just ip, _, Just lastTouch) | ballPrev ^. _y >= leftTouchLineMin ^. _y && ballNow  ^. _y < leftTouchLineMax ^. _y && ip ^. _x >= leftTouchLineMin ^. _x && ip ^. _x <= leftTouchLineMax ^. _x -> 
      let lastTouchTeam = playerTeam lastTouch
          touchLineLoc = locate2D ball
      in pure $ Just (oppositionTeam lastTouchTeam, touchLineLoc)
    (_, Just ip, Just lastTouch) | ballPrev ^. _y <= rightTouchLineMin ^. _y && ballNow  ^. _y > rightTouchLineMax ^. _y && ip ^. _x >= rightTouchLineMin ^. _x && ip ^. _x <= rightTouchLineMax ^. _x -> 
      let lastTouchTeam = playerTeam lastTouch
          touchLineLoc = locate2D ball
      in pure $ Just (oppositionTeam lastTouchTeam, touchLineLoc)
    _ -> pure Nothing
  
checkForThrowIn :: (Match m, Monad m, Log m) => m ()
checkForThrowIn = do
  maybeThrow <- wentForThrowIn
  case maybeThrow of
    Just (team, loc) -> do
      _ <- setBallMotionParams (V3 (fst loc) (snd loc) 0) (V3 0 0 0)
      setGameState $ ThrowIn team loc
    Nothing -> pure ()
