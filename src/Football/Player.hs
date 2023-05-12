module Football.Player where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, signorm, dot, quadrance))
import Football.Ball
import GHC.IO (unsafePerformIO)
import Data.List (sort)

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

maxMag :: Double -> V3 Double -> V3 Double
maxMag m v =
  if norm v > m then
    signorm v * V3 m m m
  else
    v

  
interceptionVector :: Double -> (Double, Double) -> V3 Double -> (Double, Double) -> V3 Double -> V3 Double
interceptionVector speed (tx, ty) tv (px, py) pv =
  let a = quadrance tv - speed ** 2.0
      b = 2.0 * dot (V3 tx ty 0 - V3 px py 0) tv
      c = quadrance (V3 tx ty 0 - V3 px py 0)
      root1 = (-b + sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
      root2 = (-b - sqrt(b ** 2.0  - 4 * a * c / 2)) / (2 * a)
      ts = sort . filter (>0) $ [root1, root2]
      
  in case ts of 
    t : _ -> signorm (V3 tx ty 0 + pure t * (tv - pv) - V3 px py 0)
    []    -> tv

