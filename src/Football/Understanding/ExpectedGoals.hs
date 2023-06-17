module Football.Understanding.ExpectedGoals where

import Football.Match
import Football.Locate2D (Locate2D (locate2D))
import Football.Pitch (isInPitchBounds)
import Football.Types
import Football.Understanding.Team (toTeamCoordinateSystem)
import Linear (V2(V2))

xgValue :: Double -> Double
xgValue distance =
    min 0.99 $ 1.123*exp(-0.203*distance)+0.023

locationXG :: (Match m, Monad m, Locate2D l) => Team -> l -> m Double
locationXG team l = do
  pitch' <- pitch
  let halfLengthOfPitch = pitchLength pitch' / 2
  attackingDirection' <- attackingDirection team
  let (V2 x y) = locate2D l
  if isInPitchBounds (x, y) pitch' then 
    pure $ case attackingDirection' of
      AttackingLeftToRight -> xgValue (sqrt ((x + halfLengthOfPitch)**2.0 + y **2.0))
      AttackingRightToLeft -> xgValue (sqrt ((halfLengthOfPitch - x)**2.0 + y **2.0))
  else
    pure 0



