module Football.Pitch where


data Pitch = Pitch
  { pitchLength :: Double
  , pitchWidth :: Double
  }

pitchHalfwayLineX :: Pitch -> Double
pitchHalfwayLineX pitch = pitchLength pitch / 2


