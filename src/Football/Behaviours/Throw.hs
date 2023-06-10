module Football.Behaviours.Throw where

import Football.Types
import Linear (V3)
import Football.Match
import Core (Log)
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Behaviours.Kick (timeForPassTo, motionVectorForPassTo)
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Interception (interceptionTimePlayerBallRK, interceptionTimePlayersBallRK)
import Control.Monad (filterM)

data ThrowDesirability = ThrowDesirability
  { throwTarget :: !Player
  , throwBallVector :: !(V3 Double)
  , throwOppositionInterceptionDistance :: !Double
  , throwTeammateReceptionDistance :: !Double
  -- | Approximate probability of successful throw completion
  , throwSafetyCoeff :: !Double
  -- | Approximate XG added by the pass
  , throwXGAdded :: !Double
  -- | Approximate opposition XG added by the pass (useful for judging e.g. how dangerous a backwards throw is)
  , throwOppositionXGAdded :: !Double
  } deriving (Eq, Show)


shortThrowOptions :: (Monad m, Match m, Log m) => Player -> m [ThrowDesirability]
shortThrowOptions player = do
  pitch' <- pitch
  teamPlayers' <- teammates player
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) player
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) player
  let calcToFeetDesirability p1 = do
        let t = timeForPassTo ball $ locate2D p1 
            ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D (playerPositionVector p1 + playerMotionVector p1 * pure t) } 
        
        trd <- interceptionTimePlayerBallRK p1 ball'
        oid <- interceptionTimePlayersBallRK oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1
        pure $ ThrowDesirability 
          { throwTarget = p1
          , throwBallVector = ballMotionVector ball'
          , throwOppositionInterceptionDistance = oid
          , throwTeammateReceptionDistance = trd
          , throwSafetyCoeff = safety
          , throwXGAdded = (newXG - originalXG)*safety
          , throwOppositionXGAdded = (newOppXG - originalOppXG)*(1-safety)
          }
  traverse calcToFeetDesirability teamPlayers'

throwOptions :: (Monad m, Match m, Log m) => Player -> m [ThrowDesirability]
throwOptions player = shortThrowOptions player

