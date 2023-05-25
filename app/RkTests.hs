module RkTests where

import Football.Ball (rungeKutte, ballMotionEq, Ball(..))
import Linear (Metric(norm, distance), V3 (V3))
import Data.Foldable (traverse_)
import GHC.IO (unsafePerformIO)

ballDistanceTimeUntil2mps :: Ball -> (Double, Double)
ballDistanceTimeUntil2mps ball =
  let bpv = ballPositionVector ball
      bmv = ballMotionVector ball
      droppity (t', (bpv', bmv')) =
        norm bmv' >= 5.0
      (t, (fbpv, fbmv)) = head $ dropWhile droppity $ rungeKutte (bpv, bmv) 0.1 ballMotionEq
  in (distance fbpv bpv, t)

trySomeBalls :: IO ()
trySomeBalls = do
  let speeds = [0.5,0.6..31]
  --let speeds = [5]
  let balls = (\speed -> Ball {ballPositionVector = V3 0 0 0, ballMotionVector = V3 1 0 0 * pure speed} ) <$> speeds
  let distances = ballDistanceTimeUntil2mps <$> balls 
  traverse_ (\(s, (dist, time)) -> putStrLn $ show s <> ", " <> show dist <> ", " <> show time)  $ zip speeds distances


