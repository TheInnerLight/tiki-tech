module Football.Pitch where
import Football.Locate2D (Locate2D (locate2D))



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
  
