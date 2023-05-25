module Football.Understanding.Shape where


import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm, distance))
import Data.List (sortOn, minimumBy, reverse, foldl', find)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV, spacePolyPlayer), SpaceMap (SpaceMap))
import qualified Data.Map as Map
import Core (Log(..))
import Football.Behaviours.Kick (motionVectorForPassTo)
import Data.Maybe (isNothing)
import Football.Understanding.Space (centreOfPlay, offsideLine)

data PositionSphere = PositionEllipse
    { positionEllipseCentre :: (Double, Double)
    , positionEllipseXAxis :: Double
    , positionEllipseYAxis :: Double
    }

outOfPossessionDesiredPosition :: (Monad m, Match m, Log m) => Player -> m (Double, Double)
outOfPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- centreOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  let centreX = 0.85*pCentreX+0.15*ballX
  let centreY = max 28 $ min 40 $ 0.85*pCentreY+ 0.15*ballY
  attackingDirection' <- attackingDirection (playerTeam player)
  offsideLineX <- offsideLine (playerTeam player)
  let pos = case playerNumber player of
        1 -> inDirection attackingDirection' 25     (-35) 0     (offsideLineX, centreY)
        2 -> inDirection attackingDirection' 62.5   (-25) 20    (offsideLineX, centreY)
        3 -> inDirection attackingDirection' 62.5   (-25) (-20) (offsideLineX, centreY)
        4 -> inDirection attackingDirection' 52.5   (-25) (10)  (offsideLineX, centreY)
        5 -> inDirection attackingDirection' 52.5   (-25) (-10) (offsideLineX, centreY)
        6 -> inDirection attackingDirection' 65     (-20) 0     (offsideLineX, centreY)
        10 -> inDirection attackingDirection' 70    (-15) (-10) (offsideLineX, centreY)
        8 -> inDirection attackingDirection' 70     (-15) (10)  (offsideLineX, centreY)
        11 -> inDirection attackingDirection' 85    (-3)  (-15) (offsideLineX, centreY)
        7 -> inDirection attackingDirection' 85     (-3)  (15)  (offsideLineX, centreY)
        9 -> inDirection attackingDirection' 90     (-1)  0     (offsideLineX, centreY)
        _ -> inDirection attackingDirection' 105     0    0     (offsideLineX, centreY)
  clampPitch pos
  where
    inDirection AttackingLeftToRight maxX diffX diffY (x, y) = (min maxX $ x+diffX, y+diffY)
    inDirection AttackingRightToLeft maxX diffX diffY (x, y) = (max (105-maxX) $ x-diffX, y-diffY)

inPossessionDesiredPosition :: (Monad m, Match m, Log m) => Player -> m (Double, Double)
inPossessionDesiredPosition player = do
  (pCentreX, pCentreY) <- centreOfPlay
  (ballX, ballY) <- locate2D <$> gameBall
  offsideLineX <- offsideLine (playerTeam player)
  let centreX = 0.75*pCentreX+0.25*ballX
  let centreY = max 28 $ min 40 $ 0.75*pCentreY+0.25*ballY
  attackingDirection' <- attackingDirection (playerTeam player)
  let pos = case playerNumber player of
            1 -> inDirection attackingDirection' 25 (-30) 0 (centreX, centreY)
            2 -> inDirection attackingDirection' offsideLineX  (5) 30 (centreX, centreY)
            3 -> inDirection attackingDirection' offsideLineX  (5) (-30) (centreX, centreY)
            4 -> inDirection attackingDirection' offsideLineX  (-15) 10 (centreX, centreY)
            5 -> inDirection attackingDirection' offsideLineX  (-15) (-10) (centreX, centreY)
            6 -> inDirection attackingDirection' offsideLineX  (-5) 0 (centreX, centreY)
            10 -> inDirection attackingDirection' offsideLineX (0) (-15) (centreX, centreY)
            8 -> inDirection attackingDirection' offsideLineX  (0) 15 (centreX, centreY)
            11 -> inDirection attackingDirection' offsideLineX (10) (-20) (centreX, centreY)
            7 -> inDirection attackingDirection' offsideLineX  (10) 20 (centreX, centreY)
            9 -> inDirection attackingDirection' offsideLineX  (10) 0 (centreX, centreY)
            _ -> inDirection attackingDirection' offsideLineX   0 0 (centreX, centreY)
  clampPitch pos
  where
    inDirection AttackingLeftToRight maxX diffX diffY (x, y) = (min maxX $ x+diffX, y+diffY)
    inDirection AttackingRightToLeft maxX diffX diffY (x, y) = (max maxX $ x-diffX, y-diffY)
