{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Football.Understanding.Shape where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm, distance))
import Data.List (sortOn, minimumBy, reverse, foldl', find)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import qualified Data.Map as Map
import Core (Log(..), Cache)
import Football.Understanding.Space (centresOfPlay, offsideLine)
import Football.Pitch (Pitch(pitchLength, pitchWidth), pitchHalfwayLineX)
import Football.Types
import Football.Understanding.Space.Data (CentresOfPlayCache, CentresOfPlay (centresOfPlayBothTeams, centresOfPlayTeam1, centresOfPlayTeam2))
import Football.Understanding.Team (toTeamCoordinateSystem2D, fromTeamCoordinateSystem2D, inTeamCoordinateSystem, toTeamCoordinateSystem)

outOfPossessionFormationRelativeTo :: (Monad m, Match m) => Double -> Double -> Player -> (Double, Double) -> m (Double, Double)
outOfPossessionFormationRelativeTo verticalCompactness horizontalCompactness player (uCentreX, uCentreY) = do
  (centreX, centreY) <- toTeamCoordinateSystem2D (playerTeam player) (uCentreX, uCentreY)
  let pos = case playerNumber player of
        1 -> inDirection  (-25)   (-52.5) (-25*verticalCompactness) 0                              (centreX, centreY)
        2 -> inDirection  27.5    (-52)   (-15*verticalCompactness) (8+5*horizontalCompactness)    (centreX, centreY)
        3 -> inDirection  27.5    (-52)   (-15*verticalCompactness) (-8-5*horizontalCompactness)   (centreX, centreY)
        4 -> inDirection  12.5    (-52)   (-15*verticalCompactness) (3+5*horizontalCompactness)    (centreX, centreY)
        5 -> inDirection  12.5    (-52)   (-15*verticalCompactness) (-3-5*horizontalCompactness)   (centreX, centreY)
        6 -> inDirection  25      (-52)   (-10*verticalCompactness) 0                              (centreX, centreY)
        10 -> inDirection 37      (-52)   (-5*verticalCompactness)  (-3-5*horizontalCompactness)   (centreX, centreY)
        8 -> inDirection  37      (-52)   (-5*verticalCompactness)  (3+5*horizontalCompactness)    (centreX, centreY)
        11 -> inDirection 47      (-52)   (7*verticalCompactness)   (-5-5*horizontalCompactness)   (centreX, centreY)
        7 -> inDirection  47      (-52)   (7*verticalCompactness)   (5+5*horizontalCompactness)    (centreX, centreY)
        9 -> inDirection  52      (-52)   (9*verticalCompactness)   0                              (centreX, centreY)
        _ -> inDirection  52      0  0                              0                              (centreX, centreY)
  fromTeamCoordinateSystem2D (playerTeam player) pos
  where
    inDirection maxX minX diffX diffY (x, y) = (max minX $ min maxX $ x+diffX, y+diffY)


outOfPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (Double, Double)
outOfPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- case oppositionTeam (playerTeam player) of
    Team1 -> centresOfPlayTeam1 <$> centresOfPlay
    Team2 -> centresOfPlayTeam2 <$> centresOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  pitch' <- pitch

  let (tCentreX, tCentreY) = (0.55*pCentreX+0.45*ballX, max (-15) $ min 15 $ 0.85*pCentreY+ 0.15*ballY)
      halfPitchLength = 0.5 * pitchLength pitch'
      horizontalCompactness = min halfPitchLength (halfPitchLength + tCentreX) / halfPitchLength
  pos <- outOfPossessionFormationRelativeTo 1 horizontalCompactness player (pCentreX, pCentreY)
  clampPitch pos

inPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (Double, Double)
inPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- centresOfPlayBothTeams <$> centresOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  pitch' <- pitch
  offsideLineX <- offsideLine (playerTeam player)
  attackingDirection' <- attackingDirection (playerTeam player)

  (centreX, centreY) <- toTeamCoordinateSystem2D (playerTeam player) (0.6*pCentreX+0.4*ballX, max (-15) $ min 15 $ 0.85*pCentreY + 0.15*ballY)

  let offsideLineX' =
        case attackingDirection' of
          AttackingLeftToRight -> offsideLineX
          AttackingRightToLeft -> -offsideLineX
      halfPitchLength = 0.5 * pitchLength pitch'
      horizontalCompactness = min halfPitchLength (halfPitchLength + centreX) / halfPitchLength
    
  let pos = case playerNumber player of
            1 -> inDirection                                    25 (-30) 0                           (centreX, centreY)
            2 -> inDirection  offsideLineX'                     (5) 30                               (centreX, centreY)
            3 -> inDirection  offsideLineX'                     (5) (-30)                            (centreX, centreY)
            4 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) 10                             (centreX, centreY)
            5 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) (-10)                          (centreX, centreY)
            6 -> inDirection  (pitchHalfwayLineX pitch' + 17.5) (-5)  0                              (centreX, centreY)
            10 -> inDirection (pitchHalfwayLineX pitch' + 25)   (0)   (-15)                          (centreX, centreY)
            8 -> inDirection  (pitchHalfwayLineX pitch' + 25)   (0)   15                             (centreX, centreY)
            11 -> inDirection offsideLineX'                     (15)  (-8-12*horizontalCompactness)  (centreX, centreY)
            7 -> inDirection  offsideLineX'                     (15)  (8+12*horizontalCompactness)   (centreX, centreY)
            9 -> inDirection  offsideLineX'                     (15)  0                              (centreX, centreY)
            _ -> inDirection  offsideLineX'                     0     0                              (centreX, centreY)

  pos' <- fromTeamCoordinateSystem2D (playerTeam player) pos
  clampPitch pos'
  where
    inDirection maxX diffX diffY (x, y) = (min maxX $ x+diffX, y+diffY)
