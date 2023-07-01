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
  playerState <- getPlayerState player
  teamPlayers' <- teammates player
  ball <- gameBall
  oppositionPlayers' <- oppositionPlayers (playerTeam player)
  originalXG <- locationXG (playerTeam player) playerState
  originalOppXG <- locationXG (oppositionTeam $ playerTeam player) playerState
  let calcToFeetDesirability p1State = do
        let t = timeForPassTo ball $ locate2D p1State
            ball' = ball { ballMotionVector = motionVectorForPassTo ball $ locate2D (playerStatePositionVector p1State + playerStateMotionVector p1State * pure t) } 
        
        trd <- interceptionTimePlayerBallRK False p1State ball'
        oid <- interceptionTimePlayersBallRK True oppositionPlayers' ball'
        let z1 = (oid - trd) / sqrt 2
            a = 4.68
            b = 0.48
            safety = 1 / (1 + exp (-(a * z1 + b))) 
        newXG <- locationXG (playerTeam player) p1State
        newOppXG <- locationXG (oppositionTeam $ playerTeam player) p1State
        pure $ ThrowDesirability 
          { throwTarget = playerStatePlayer p1State
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

