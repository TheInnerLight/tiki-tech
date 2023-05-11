module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot))
import Football.Ball
import GHC.IO (unsafePerformIO)

data PlayerIntention
  = KickIntention (Double, Double)

data PlayerSpeed = PlayerSpeed
  { playerSpeedAcceleration :: Double
  , playerSpeedMax :: Double
  }

data Player = Player
  { playerPosition :: (Double, Double)
  , playerNumber :: Int
  , playerSpeed :: PlayerSpeed
  , playerMotionVector :: V3 Double
  , playerIntention :: PlayerIntention
  }

updatePlayer :: Double -> Player -> Player
updatePlayer dt player =
  let (px, py) = playerPosition player
      px' = px + (playerMotionVector player ^. _x / dt)
      py' = py + (playerMotionVector player ^. _y / dt)
      --playerUnitDirection = signorm $ playerMotionVector player
      playerMotionVector' = playerMotionVector player
  in player { playerPosition = (px', py'), playerMotionVector = playerMotionVector' }

updateIntention :: Ball -> Player -> Player
updateIntention ball player =
  let (px, py) = playerPosition player
      (bx, by) = ballPosition ball
      --direction = signorm $ V3 (bx-px) (by-py) 0
      direction = interceptionVector2 (bx, by) (ballMotionVector ball) (px, py)

      motion = playerMotionVector player
      motion' = maxMag (playerSpeedMax $ playerSpeed player) $ (motion + direction * pure (playerSpeedAcceleration $ playerSpeed player))
  in player { playerMotionVector = motion' }

maxMag :: Double -> V3 Double -> V3 Double
maxMag m v =
  if norm v > m then
    signorm v * V3 m m m
  else
    v

  
interceptionVector2 :: (Double, Double) -> V3 Double -> (Double, Double) -> V3 Double
interceptionVector2 (tx, ty) tv (px, py) =
  let a = dot tv tv - dot (pure 5 :: V3 Double) (pure 5 :: V3 Double)
      b = 2.0 * dot (V3 tx ty 0 - V3 px py 0) tv
      c = dot (V3 tx ty 0 - V3 px py 0) (V3 tx ty 0 - V3 px py 0)
      root1 = -b + sqrt(b ** 2.0  - 4 * a * c / 2) / (2 * a)
      root2 = -b - sqrt(b ** 2.0  - 4 * a * c / 2) / (2 * a)
      
  in if root1 >= 0 && root2 <= 0 then
      signorm (V3 tx ty 0 + pure root1 * tv - V3 px py 0)
    else if root2 >= 0 && root1 <= 0 then
      signorm (V3 tx ty 0 + pure root2 * tv - V3 px py 0)
    else if root1 >= 0 && root2 >= 0 then
      signorm (V3 tx ty 0 + pure (min root1 root2) * tv - V3 px py 0)
    else 
      V3 0 0 0


