module Football.Pitch where
import Football.Locate2D (Locate2D (locate2D))
import Linear (V3(..), V2 (V2))
import Football.Types

pitchHalfwayLineX :: Pitch -> Double
pitchHalfwayLineX _ = 0

pitchHalfLengthX :: Pitch -> Double
pitchHalfLengthX pitch = pitchLength pitch / 2

pitchHalfWidthY :: Pitch -> Double
pitchHalfWidthY pitch = pitchWidth pitch / 2

isInPitchBounds :: (Locate2D x) => x -> Pitch -> Bool
isInPitchBounds l pitch = 
  let (V2 x y) = locate2D l
  in x >= -pitchHalfLengthX pitch && y >= -pitchHalfWidthY pitch && x <= pitchHalfLengthX pitch && y <= pitchHalfWidthY pitch
  
leftGoalLine :: Pitch -> (V3 Double, V3 Double)
leftGoalLine pitch =
  (V3 (-pitchHalfLengthX pitch) (-3.66) 0, V3 (-pitchHalfLengthX pitch) 3.66 2.44)

rightGoalLine :: Pitch -> (V3 Double, V3 Double)
rightGoalLine pitch =
  (V3 (pitchHalfLengthX pitch) (-3.66) 0, V3 (pitchHalfLengthX pitch) 3.66 2.44)

