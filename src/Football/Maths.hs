module Football.Maths where
import Linear (V3 (V3), normalize, Metric (dot, quadrance, norm, distance), cross)

linePlaneIntersection :: (V3 Double, V3 Double) -> (V3 Double, V3 Double) -> Maybe (V3 Double)
linePlaneIntersection (l0, l) (p0, n) =
  let l' = normalize l
      n' = normalize n
  in 
    if (p0 - l0) `dot` n' == 0 then
      Nothing
    else
      let d = (p0-l0) `dot` n' / l' `dot` n'
      in Just $ l0 + pure d * l'

distanceAndClosestIntercepts ::  Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double) -> (Double, V3 Double, V3 Double)
distanceAndClosestIntercepts dt (o1p, o1m) (o2p, o2m) =
  let o1Dir = - normalize o1m
      o2Dir = - normalize o2m
      n = o1Dir `cross` o2Dir
      t1 = (o2Dir `cross` n) `dot` (o2p - o1p) / quadrance n
      t2 = (o1Dir `cross` n) `dot` (o2p - o1p) / quadrance n

      t1' = max 0 $ min (norm o2m * dt) t1
      t2' = max 0 $ min (norm o1m * dt) t2

      closestPlayerPos = o2p + pure t1' * o2Dir
      closestBallPos = o1p + pure t2' * o2Dir
  in (distance closestPlayerPos closestBallPos, closestPlayerPos, closestBallPos)

movingObjectAndPointClosestIntercept ::  Double -> (V3 Double, V3 Double) -> V3 Double -> (Double, V3 Double)
movingObjectAndPointClosestIntercept dt (o1p, o1m) o2p =
  let o1Dir = - normalize o1m
      n = o1Dir `cross` (V3 0 0 1)
      t = (o2p - o1p) `dot` n / norm n
      t' = max 0 $ min (norm o1m * dt) t
      closestPos = o1p + pure t' * o1Dir
  in (distance closestPos o2p, closestPos)

