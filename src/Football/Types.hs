module Football.Types where

import Linear (V3(..), V2)

data Team
  = Team1
  | Team2
  deriving (Eq, Ord, Show)

oppositionTeam :: Team -> Team
oppositionTeam Team1 = Team2
oppositionTeam Team2 = Team1

data PlayerIntention
  = PassIntention         PassTarget    (V2 Double)   (V3 Double)   GameTime
  | ThrowIntention        Player        (V2 Double)   (V3 Double)
  | TakeCornerIntention   PassTarget    (V2 Double)   (V3 Double)
  | TakeGoalKickIntention PassTarget    (V2 Double)   (V3 Double)
  | TakeKickOffIntention  PassTarget    (V2 Double)   (V3 Double)
  | ShootIntention        ShotTarget    (V2 Double)   (V3 Double)
  | DribbleIntention      (V2 Double)   (V3 Double)
  | MoveIntoSpace         (V2 Double)   GameTime
  | RunToLocation         (V2 Double)   GameTime
  | ControlBallIntention  (V2 Double)   GameTime
  | IntentionCooldown     GameTime
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
  | SpaceTarget (V2 Double)
  deriving (Eq, Ord, Show)

data ShotTarget 
  = CentreShot (V2 Double)
  deriving (Eq, Ord, Show)

data Goal = Goal
  { goalTeam :: !Team
  , goalScorer :: !Player
  , goalTime :: !GameTime
  } deriving (Eq, Ord, Show)

data PhaseOfPlay 
  = AttackingTransitionPhase
  | DefensiveTransitionPhase
  | InPossessionPhase
  | OutOfPossessionPhase
  deriving (Eq, Ord, Show)

data GameState
  = OpenPlay
  | ThrowIn Team (V2 Double)
  | CornerKick Team (V2 Double)
  | GoalKick Team (V2 Double)
  | KickOff Team
  deriving Eq
  
data GameHalf
  = FirstHalf
  | SecondHalf
  deriving (Eq, Ord, Show)

data GameTime 
  = GameTime GameHalf Int
  deriving (Eq, Ord, Show)

data TouchOfBall = TouchOfBall
  { touchOfBallPlayer :: !Player
  , touchOfBallTime :: !GameTime
  }

data MatchLogEntry
  = GoalLogEntry Goal
  | TouchLogEntry TouchOfBall


data Pitch = Pitch
  { pitchLength :: Double
  , pitchWidth :: Double
  }


