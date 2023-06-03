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

distanceAndClosestInterceptsWithinTimeStep ::  Double -> (V3 Double, V3 Double) -> (V3 Double, V3 Double) -> (Double, V3 Double, V3 Double)
distanceAndClosestInterceptsWithinTimeStep dt (o1p, o1m) (o2p, o2m) =
  let (o1Dir, o2Dir) = 
        if dt < 0 then (- normalize o1m, - normalize o2m)
        else (normalize o1m, normalize o2m)
      dt' = abs dt
      n = o1Dir `cross` o2Dir
      t2 = (o1Dir `cross` n) `dot` (o2p - o1p) / quadrance n
      t1 = (o2Dir `cross` n) `dot` (o2p - o1p) / quadrance n
      

      t1' = max 0 $ min (norm o1m * dt') t1
      t2' = max 0 $ min (norm o2m * dt') t2

      closestO1Pos = o1p + pure t1' * o1Dir
      closestO2Pos = o2p + pure t2' * o2Dir
  in (distance closestO1Pos closestO2Pos, closestO1Pos, closestO2Pos)

movingObjectAndPointClosestInterceptWithinTimeStep ::  Double -> (V3 Double, V3 Double) -> V3 Double -> (Double, V3 Double)
movingObjectAndPointClosestInterceptWithinTimeStep dt (o1p, o1m) o2p =
  let o1Dir = 
        if dt < 0 then - normalize o1m
        else normalize o1m
      dt' = abs dt
      n = o1Dir `cross` (V3 0 0 1)
      t = (o2p - o1p) `dot` n / norm n
      t' = max 0 $ min (norm o1m * dt') t
      closestPos = o1p + pure t' * o1Dir
  in (distance closestPos o2p, closestPos)

