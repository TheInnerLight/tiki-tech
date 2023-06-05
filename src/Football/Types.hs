module Football.Types where

import Linear (V3(..))
import Data.Time.Clock.System (SystemTime)

data Team
  = Team1
  | Team2
  deriving (Eq, Ord, Show)

oppositionTeam :: Team -> Team
oppositionTeam Team1 = Team2
oppositionTeam Team2 = Team1

data PlayerIntention
  = PassIntention PassTarget (Double, Double) (V3 Double)
  | ShootIntention ShotTarget (Double, Double) (V3 Double)
  | DribbleIntention (Double, Double) (V3 Double)
  | MoveIntoSpace (Double, Double) SystemTime
  | RunToLocation (Double, Double) SystemTime
  | ControlBallIntention (Double, Double) SystemTime
  | IntentionCooldown SystemTime
  | DoNothing
  deriving (Eq, Ord, Show)

data PlayerSpeed = PlayerSpeed
  { playerSpeedAcceleration :: !Double
  , playerSpeedMax :: !Double
  }
  deriving (Eq, Ord, Show)

data Player = Player
  { playerPositionVector :: !(V3 Double)
  , playerNumber :: !Int
  , playerSpeed :: !PlayerSpeed
  , playerMotionVector :: !(V3 Double)
  , playerIntention :: !PlayerIntention
  , playerTeam :: !Team
  }
  deriving (Eq, Ord, Show)

data Ball = Ball
  { ballPositionVector :: !(V3 Double)
  , ballMotionVector :: !(V3 Double)
  } deriving (Eq, Ord, Show)

data PassTarget
  = PlayerTarget Player
  | AheadOfTarget Player
  | SpaceTarget (Double, Double)
  deriving (Eq, Ord, Show)

data ShotTarget 
  = CentreShot (Double, Double)
  deriving (Eq, Ord, Show)

data Goal = Goal
  { goalTeam :: !Team
  , goalScorer :: !Player
  } deriving (Eq, Ord, Show)

data PhaseOfPlay 
  = AttackingTransitionPhase
  | DefensiveTransitionPhase
  | InPossessionPhase
  | OutOfPossessionPhase
  


