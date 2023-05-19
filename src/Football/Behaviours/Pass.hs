module Football.Behaviours.Pass where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm))
import Data.List (sortOn, minimumBy, reverse)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV), SpaceMap (SpaceMap))
import qualified Data.Map as Map

data PassTarget
  = PlayerTarget Player
  | SpaceTarget (Double, Double)
  deriving Show

data PassDesirability = PassDesirability
  { passTarget :: PassTarget
  , passOppositionInterceptionDistance :: Double
  , passTeammateReceptionDistance :: Double
  , passSafetyCoeff :: Double
  } deriving Show

safestPassingOptions :: (Monad m, Match m) => Player -> m [PassDesirability]
safestPassingOptions player = do
  teamPlayers' <- teammates player
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  teamSpaceMap <- spaceMapForTeam player
  ball <- gameBall
  let calcToFeetDesirability p1 =
        let ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D p1 }
            trd = minimum $ fmap (`interceptionTimePlayerBall` ball') teamPlayers'
            oid = minimum $ fmap (`interceptionTimePlayerBall` ball') oppositionPlayers'
            z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
        in PassDesirability 
          { passTarget = PlayerTarget p1
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = 1 / (1 + exp (-(a * z1 + b)))
          }
      calcToSpaceDesirability v1 =
        let (centreX, centreY) = polyPoint v1
            ball' = ball { ballMotionVector = motionVectorForPassTo ball (centreX, centreY) }
            trd = minimum $ fmap (`interceptionTimePlayerBall` ball') teamPlayers'
            oid = minimum $ fmap (`interceptionTimePlayerBall` ball') oppositionPlayers'
            z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
        in PassDesirability 
          { passTarget = SpaceTarget (centreX, centreY)
          , passOppositionInterceptionDistance = oid
          , passTeammateReceptionDistance = trd
          , passSafetyCoeff = 1 / (1 + exp (-(a * z1 + b)))
          }
  let toFeetPassOptions = fmap calcToFeetDesirability teamPlayers'
  let toSpacePassOptions = fmap calcToSpaceDesirability $ spacePolyJCV <$> teamSpaceMap
  pure $ sortOn (Data.Ord.Down . passSafetyCoeff) $ toFeetPassOptions ++ toSpacePassOptions

motionVectorForPassTo :: Ball -> (Double, Double) -> V3 Double
motionVectorForPassTo ball (targetX, targetY) = 
  ballDirection * pure (min 31 $ dist ** 0.35 * 4.5) - ballMotionVector ball
  where
    targetVector = V3 targetX targetY 0
    ballDirection = normalize (targetVector - ballPositionVector ball - ballMotionVector ball)
    dist = norm (targetVector - ballPositionVector ball)

