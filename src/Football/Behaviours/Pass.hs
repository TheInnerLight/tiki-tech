module Football.Behaviours.Pass where

import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm))
import Data.List (sortOn, minimumBy, reverse)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Space.Data (SpacePoly(spacePolyJCV, spacePolyPlayer), SpaceMap (SpaceMap))
import qualified Data.Map as Map
import Core (Log(..))
import Football.Behaviours.Kick (motionVectorForPassTo)

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

safestPassingOptions :: (Monad m, Match m, Log m) => Player -> m [PassDesirability]
safestPassingOptions player = do
  teamPlayers' <- teammates player
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  teamSpaceMap <- spaceMapForTeam player
  ball <- gameBall
  let calcToFeetDesirability p1 =
        let ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D p1 }
            trd = interceptionTimePlayersBallRK teamPlayers' ball'
            --trd = minimum $ fmap (`interceptionTimePlayerBallRK` ball') teamPlayers'
            oid = interceptionTimePlayersBallRK oppositionPlayers' ball'
            --oid = minimum $ fmap (`interceptionTimePlayerBallRK` ball') oppositionPlayers'
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
            trd = interceptionTimePlayersBallRK teamPlayers' ball'
            --trd = minimum $ fmap (`interceptionTimePlayerBallRK` ball') teamPlayers'
            oid = interceptionTimePlayersBallRK oppositionPlayers' ball'
            --oid = minimum $ fmap (`interceptionTimePlayerBallRK` ball') oppositionPlayers'
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
  let toSpacePassOptions = fmap calcToSpaceDesirability $ spacePolyJCV <$> filter (\p -> spacePolyPlayer p /= player) teamSpaceMap
  let res = sortOn (Data.Ord.Down . passSafetyCoeff) (toFeetPassOptions ++ toSpacePassOptions)
  --logOutput res
  pure res


