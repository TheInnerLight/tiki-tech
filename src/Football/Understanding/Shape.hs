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
import Core (Log(..))
import Football.Understanding.Space (centreOfPlay, offsideLine)
import Football.Pitch (Pitch(pitchLength, pitchWidth), pitchHalfwayLineX)

data PositionSphere = PositionEllipse
    { positionEllipseCentre :: (Double, Double)
    , positionEllipseXAxis :: Double
    , positionEllipseYAxis :: Double
    }

outOfPossessionDesiredPosition :: (Monad m, Match m, Log m) => Player -> m (Double, Double)
outOfPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- centreOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  pitch' <- pitch

  attackingDirection' <- attackingDirection (playerTeam player)
  let (centreX, centreY) = 
        case attackingDirection' of
          AttackingLeftToRight -> (0.55*pCentreX+0.45*ballX                        , max 28 $ min 40 $ 0.85*pCentreY+ 0.15*ballY                       )
          AttackingRightToLeft -> (pitchLength pitch' - (0.55*pCentreX+0.45*ballX) , max 28 $ min 40 $ pitchWidth pitch' - (0.85*pCentreY + 0.15*ballY))
      --centreY = max 28 $ min 40 $ 0.85*pCentreY+ 0.15*ballY
      halfPitchLength = 0.5 * pitchLength pitch'
      widthMult = min halfPitchLength centreX / halfPitchLength

  let pos = case playerNumber player of
        1 -> inDirection  25   1     (-25) 0                 (centreX, centreY)
        2 -> inDirection  62.5 5     (-15) (8+5*widthMult)   (centreX, centreY)
        3 -> inDirection  62.5 5     (-15) (-8-5*widthMult)  (centreX, centreY)
        4 -> inDirection  52.5 5     (-15) (3+5*widthMult)   (centreX, centreY)
        5 -> inDirection  52.5 5     (-15) (-3-5*widthMult)  (centreX, centreY)
        6 -> inDirection  65   10    (-10) 0                 (centreX, centreY)
        10 -> inDirection 70   15    (-5) (-3-5*widthMult)   (centreX, centreY)
        8 -> inDirection  70   15    (-5) (3+5*widthMult)    (centreX, centreY)
        11 -> inDirection 85   20    (7)  (-5-5*widthMult)   (centreX, centreY)
        7 -> inDirection  85   20    (7)  (5+5*widthMult)    (centreX, centreY)
        9 -> inDirection  90   25    (9)  0                  (centreX, centreY)
        _ -> inDirection  105  0     0    0                  (centreX, centreY)

  let pos' = 
        case attackingDirection' of
          AttackingLeftToRight -> pos
          AttackingRightToLeft -> (pitchLength pitch' - fst pos, pitchWidth pitch' - snd pos)

  clampPitch pos'
  where
    inDirection maxX minX diffX diffY (x, y) = (max minX $ min maxX $ x+diffX, y+diffY)

inPossessionDesiredPosition :: (Monad m, Match m, Log m) => Player -> m (Double, Double)
inPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- centreOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  pitch' <- pitch
  offsideLineX <- offsideLine (playerTeam player)
  attackingDirection' <- attackingDirection (playerTeam player)
  let (centreX, centreY, offsideLineX') = 
        case attackingDirection' of
          AttackingLeftToRight -> (0.6*pCentreX+0.4*ballX                       , max 28 $ min 40 $ 0.85*pCentreY+ 0.15*ballY                      , offsideLineX                     )
          AttackingRightToLeft -> (pitchLength pitch' - (0.6*pCentreX+0.4*ballX), max 28 $ min 40 $ pitchWidth pitch' - (0.85*pCentreY+ 0.15*ballY), pitchLength pitch' - offsideLineX)
      halfPitchLength = 0.5 * pitchLength pitch'
      widthMult = (pitchLength pitch' - max halfPitchLength centreX) / halfPitchLength
    
  let pos = case playerNumber player of
            1 -> inDirection                                    25 (-30) 0               (centreX, centreY)
            2 -> inDirection  offsideLineX'                     (5) 30                   (centreX, centreY)
            3 -> inDirection  offsideLineX'                     (5) (-30)                (centreX, centreY)
            4 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) 10                 (centreX, centreY)
            5 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) (-10)              (centreX, centreY)
            6 -> inDirection  (pitchHalfwayLineX pitch' + 17.5) (-5)  0                  (centreX, centreY)
            10 -> inDirection (pitchHalfwayLineX pitch' + 25)   (0)   (-15)              (centreX, centreY)
            8 -> inDirection  (pitchHalfwayLineX pitch' + 25)   (0)   15                 (centreX, centreY)
            11 -> inDirection offsideLineX'                     (15)  (-8-12*widthMult)  (centreX, centreY)
            7 -> inDirection  offsideLineX'                     (15)  (8+12*widthMult)   (centreX, centreY)
            9 -> inDirection  offsideLineX'                     (15)  0                  (centreX, centreY)
            _ -> inDirection  offsideLineX'                     0     0                  (centreX, centreY)

  let pos' = 
        case attackingDirection' of
          AttackingLeftToRight -> pos
          AttackingRightToLeft -> (pitchLength pitch' - fst pos, pitchWidth pitch' - snd pos)

  clampPitch pos'
  where
    inDirection maxX diffX diffY (x, y) = (min maxX $ x+diffX, y+diffY)
    -- inDirection AttackingLeftToRight maxX diffX diffY (x, y) = (min maxX $ x+diffX, y+diffY)
    -- inDirection AttackingRightToLeft maxX diffX diffY (x, y) = (max maxX $ x-diffX, y-diffY)
