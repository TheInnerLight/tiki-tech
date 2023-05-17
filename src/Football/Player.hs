module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance), normalize)
import Football.Ball
import Data.List (sort)
import Data.Time.Clock.System (SystemTime)

data Team
  = Team1
  | Team2
  deriving (Eq, Show)

data PlayerIntention
  = KickIntention (Double, Double)
  | MoveIntoSpace (Double, Double)
  | ControlBallIntention
  | IntentionCooldown SystemTime
  | DoNothing
  deriving (Eq, Show)

data PlayerSpeed = PlayerSpeed
  { playerSpeedAcceleration :: Double
  , playerSpeedMax :: Double
  }
  deriving (Eq, Show)

data Player = Player
  { playerPositionVector :: V3 Double
  , playerNumber :: Int
  , playerSpeed :: PlayerSpeed
  , playerMotionVector :: V3 Double
  , playerIntention :: PlayerIntention
  , playerTeam :: Team
  }
  deriving (Eq, Show)

updatePlayer :: Double -> Player -> Player
updatePlayer dt player =
  let ppv = playerPositionVector player
      ppv' = ppv + playerMotionVector player / pure dt
      playerMotionVector' = playerMotionVector player
  in player { playerPositionVector = ppv', playerMotionVector = playerMotionVector' }

maxMag :: Double -> V3 Double -> V3 Double
maxMag m v =
  if norm v > m then
    signorm v * V3 m m m
  else
    v

interceptionTimePlayerBall :: Player -> Ball -> Double
interceptionTimePlayerBall player ball =
  case interceptionTime maxSpeed bpv (ballMotionVector ball) ppv (playerMotionVector player) of
    Just d -> d
    Nothing -> 1/0
  where 
    ppv = playerPositionVector player
    bpv = ballPositionVector ball
    maxSpeed = playerSpeedMax $ playerSpeed player

interceptionTime :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> Maybe Double
interceptionTime speed tpv tv ppv pv =
  let a = quadrance tv - speed ** 2.0
      b = 2.0 * dot (tpv - ppv) tv
      c = quadrance (tpv - ppv)
      root1 = (-b + sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
      root2 = (-b - sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
      ts = sort . filter (>0) $ [root1, root2]
  in case ts of 
    t : _ -> Just t
    []    -> Nothing
  
interceptionVector :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double
interceptionVector speed tpv tv ppv pv =
  case interceptionTime speed tpv tv ppv pv of
    Just t -> signorm (tpv + pure t * (tv - pv) - ppv)
    Nothing -> tv

