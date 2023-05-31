module Football.Pitch where
import Football.Locate2D (Locate2D (locate2D))
import Linear (V3(..))



data Pitch = Pitch
  { pitchLength :: Double
  , pitchWidth :: Double
  }

pitchHalfwayLineX :: Pitch -> Double
pitchHalfwayLineX pitch = pitchLength pitch / 2

isInPitchBounds :: (Locate2D x) => x -> Pitch -> Bool
isInPitchBounds l pitch = 
  let (x, y) = locate2D l
  in x >= 0 && y >= 0 && x <= pitchLength pitch && y <= pitchWidth pitch
  
leftGoalLine :: Pitch -> (V3 Double, V3 Double)
leftGoalLine pitch =
  (V3 0 (pitchWidth pitch / 2 - 3.66) 0, V3 0 (pitchWidth pitch / 2 + 3.66) 2.44)

rightGoalLine :: Pitch -> (V3 Double, V3 Double)
rightGoalLine pitch =
  (V3 (pitchLength pitch) (pitchWidth pitch / 2 - 3.66) 0, V3 (pitchLength pitch) (pitchWidth pitch / 2 + 3.66) 2.44)
