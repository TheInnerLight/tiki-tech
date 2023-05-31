module Football.Maths where
import Linear (V3, normalize, Metric (dot))

linePlaneIntersection :: (V3 Double, V3 Double) -> (V3 Double, V3 Double) -> Maybe (V3 Double)
linePlaneIntersection (l0, l) (p0, n) =
  let l' = normalize l
      n' = normalize n
  in 
    if dot (p0 - l0) n' == 0 then
      Nothing
    else
      let d = dot (p0-l0) n' / dot l' n'
      in Just $ l0 + pure d * l'


