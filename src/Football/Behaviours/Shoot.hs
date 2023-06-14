module Football.Behaviours.Shoot where

import Linear (V3(..), normalize)
import Football.Match
import Core (Log)
import Football.Ball
import Football.Player
import Football.Pitch
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Understanding.Interception (interceptionTimePlayerBallRK, interceptionTimePlayersBallRK)
import qualified Data.Ord
import Data.List (sortOn)
import Football.Locate2D (Locate2D(locate2D))
import Football.Types
import Football.Understanding.Team (fromTeamCoordinateSystem)

data ShotDesirability =
  ShotDesirability
    { shotTarget :: !ShotTarget
    , shotBallVector :: !(V3 Double)
    , shotXG :: !Double
    , shotSafetyCoeff :: !Double
    } deriving Show

centreShotOption :: (Monad m, Match m, Log m) => Player -> m ShotDesirability
centreShotOption player = do
  pitch' <- pitch
  targetLoc <- fromTeamCoordinateSystem (playerTeam player) $ V3 (pitchHalfLengthX pitch') 0 0
  let shotVec = 31 * normalize (targetLoc - playerPositionVector player)
  xg <- locationXG (playerTeam player) player
  pure $ ShotDesirability
    { shotTarget = CentreShot $ locate2D targetLoc
    , shotBallVector = shotVec
    , shotXG = xg
    , shotSafetyCoeff = xg
    }

desirableShotOptions :: (Monad m, Match m, Log m) => Player -> m [ShotDesirability]
desirableShotOptions player = do
  cs <- centreShotOption player
  pure [cs]
