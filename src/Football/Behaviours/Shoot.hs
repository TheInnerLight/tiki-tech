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

data ShotDesirability =
  ShotDesirability
    { shotTarget :: !ShotTarget
    , shotBallVector :: !(V3 Double)
    , shotXG :: !Double
    , shotSafetyCoeff :: !Double
    , shotDesirabilityCoeff :: !Double
    }

centreShotOption :: (Monad m, Match m, Log m) => Player -> m ShotDesirability
centreShotOption player = do
  teamPlayers' <- teammates player
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  ball <- gameBall
  attackingDirection' <- attackingDirection (playerTeam player)
  pitch' <- pitch
  let targetLoc = 
        case attackingDirection' of
          AttackingLeftToRight -> V3 (pitchLength pitch')  (pitchWidth pitch' / 2) 0
          AttackingRightToLeft -> V3 0                     (pitchWidth pitch' / 2) 0
      shotVec = 31 * normalize (targetLoc - playerPositionVector player)
  xg <- locationXG (playerTeam player) player
  pure $ ShotDesirability
    { shotTarget = CentreShot $ locate2D targetLoc
    , shotBallVector = shotVec
    , shotXG = xg
    , shotSafetyCoeff = xg
    , shotDesirabilityCoeff = xg
    }

desirableShotOptions :: (Monad m, Match m, Log m) => Player -> m [ShotDesirability]
desirableShotOptions player = do
  cs <- centreShotOption player
  pure $ sortOn (Data.Ord.Down . shotDesirabilityCoeff) [cs]
