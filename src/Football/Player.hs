module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance, distance), normalize, project)
import Football.Ball
import Data.List (sort)
import Data.Time.Clock.System (SystemTime)
import GHC.IO (unsafePerformIO)

data Team
  = Team1
  | Team2
  deriving (Eq, Ord, Show)

data PlayerIntention
  = KickIntention (Double, Double) (Double, Double)
  | MoveIntoSpace (Double, Double)
  | ControlBallIntention (Double, Double)
  | IntentionCooldown SystemTime
  | DoNothing
  deriving (Eq, Ord, Show)

data PlayerSpeed = PlayerSpeed
  { playerSpeedAcceleration :: Double
  , playerSpeedMax :: Double
  }
  deriving (Eq, Ord, Show)

data Player = Player
  { playerPositionVector :: V3 Double
  , playerDesiredLocation :: V3 Double
  , playerNumber :: Int
  , playerSpeed :: PlayerSpeed
  , playerMotionVector :: V3 Double
  , playerIntention :: PlayerIntention
  , playerTeam :: Team
  }
  deriving (Eq, Ord, Show)

updatePlayer :: Double -> Player -> Player
updatePlayer dt player =
  let ppv = playerPositionVector player
      ppv' = ppv + playerMotionVector player / pure dt
      maxSpeed = playerSpeedMax $ playerSpeed player
      acceleration = playerSpeedAcceleration $ playerSpeed player
      direction = normalize (playerDesiredLocation player - ppv')
      acc = pure maxSpeed * direction - playerMotionVector player * pure acceleration
      deltaSpeed = acc / pure dt
      pmv' = playerMotionVector player + deltaSpeed

  in player { playerPositionVector = ppv', playerMotionVector = pmv' }

maxMag :: Double -> V3 Double -> V3 Double
maxMag m v =
  if norm v > m then
    normalize v * pure m
  else
    v

-- use Fujimura-Sugihara model
distanceToTargetAfter :: V3 Double -> Double -> Player -> Double
distanceToTargetAfter target t p =
  let 
      start = playerPositionVector p
      -- vector difference between x(t) and A(t)
      st = target - start - pure ((1 - exp(-pa * t))/pa) * playerMotionVector p
      ms = playerSpeedMax (playerSpeed p)
      pa = playerSpeedAcceleration (playerSpeed p)
      -- radius B(t)
      rt = (ms*(t - (1 - exp(-pa * t))/pa))
  in norm st - rt

interceptionTimePlayerBallRK :: Player -> Ball -> Double
interceptionTimePlayerBallRK player ball = snd $ interceptionInfoPlayerBallRK player ball

interceptionInfoPlayerBallRK :: Player -> Ball -> (V3 Double, Double)
interceptionInfoPlayerBallRK player ball =
  let ppv = playerPositionVector player
      bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      droppity (t', (bpv', bmv')) = t' <= 40.0 && (distanceToTargetAfter bpv' t' player > 0.0)
      (t, (fbpv, fbmv)) = head $ dropWhile droppity $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  in (fbpv, t)

interceptionTimePlayersBallRK :: [Player] -> Ball -> Double
interceptionTimePlayersBallRK players ball = snd $ interceptionInfoPlayersBallRK players ball

interceptionInfoPlayersBallRK :: [Player] -> Ball -> (V3 Double, Double)
interceptionInfoPlayersBallRK players ball =
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      droppity (t', (bpv', bmv')) = t' <= 40.0 && all (\p -> distanceToTargetAfter bpv' t' p > 0.0) players
      (t, (fbpv, fbmv)) = head $ dropWhile droppity $ rungeKutte (bpv, bmv) 0.03 ballMotionEq
  in (fbpv, t)

runTowardsLocation :: (Double, Double) -> Player -> Player
runTowardsLocation (x, y) player =
  let targetV = V3 x y 0
  in player { playerDesiredLocation = targetV }

stopMoving :: Player -> Player
stopMoving player =
  player { playerDesiredLocation = playerPositionVector player }

-- interceptionTimePlayerBall :: Player -> Ball -> Double
-- interceptionTimePlayerBall player ball =
--   case interceptionTime maxSpeed bpv (ballMotionVector ball) ppv (playerMotionVector player) of
--     Just d -> d
--     Nothing -> 20
--   where 
--     ppv = playerPositionVector player
--     bpv = ballPositionVector ball
--     maxSpeed = playerSpeedMax $ playerSpeed player

-- interceptionTime :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> Maybe Double
-- interceptionTime speed tpv tv ppv pv =
--   let a = quadrance tv - speed ** 2.0
--       b = 2.0 * dot (tpv - ppv) tv
--       c = quadrance (tpv - ppv)
--       root1 = (-b + sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
--       root2 = (-b - sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
--       ts = sort . filter (>0) $ [root1, root2]
--   in case ts of 
--     t : _ -> Just t
--     []    -> Nothing
  
-- interceptionVector :: Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double
-- interceptionVector speed tpv tv ppv pv =
--   case interceptionTime speed tpv tv ppv pv of
--     Just t -> normalize (tpv + pure t * tv - ppv)
--     Nothing -> tv

