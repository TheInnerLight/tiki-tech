{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Football.Types where

import Linear (V3(..), V2)
import Data.TypeLits (Nat, type (+), type (==))
import qualified Data.Text as T

data TeamId
  = TeamId1
  | TeamId2
  deriving (Eq, Ord, Show)

oppositionTeam :: TeamId -> TeamId
oppositionTeam TeamId1 = TeamId2
oppositionTeam TeamId2 = TeamId1

data Team = Team
  { teamName :: T.Text
  , teamFormation :: Formation
  }

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
  { playerNumber :: !Int
  , playerSpeed :: !PlayerSpeed
  , playerTeamId :: !TeamId
  }
  deriving (Eq, Ord, Show)

data PlayerState = PlayerState
  { playerStatePlayer :: !Player
  , playerStatePositionVector :: !(V3 Double)
  , playerStateMotionVector :: !(V3 Double)
  , playerStateIntention :: !PlayerIntention
  } deriving (Eq, Ord, Show)

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
  { goalTeam :: !TeamId
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
  | ThrowIn TeamId (V2 Double)
  | CornerKick TeamId (V2 Double)
  | GoalKick TeamId (V2 Double)
  | KickOff TeamId
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

data FormationLine (n :: Nat) where 
  FiveLine  :: Player -> Player -> Player -> Player -> Player -> FormationLine 5
  FourLine  :: Player -> Player -> Player -> Player -> FormationLine 4
  ThreeLine :: Player -> Player -> Player -> FormationLine 3
  TwoLine   :: Player -> Player -> FormationLine 2
  OneLine   :: Player -> FormationLine 1
  EmptyLine :: FormationLine 0

data Formation = forall a b c d e. ((a + b + c + d + e) == 10) => Formation
  { formationLine1 :: FormationLine a
  , formationLine2 :: FormationLine b
  , formationLine3 :: FormationLine c
  , formationLine4 :: FormationLine d
  , formationLine5 :: FormationLine e
  }

