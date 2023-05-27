module Football.Understanding.ExpectedGoals where

import Football.Match
import Football.Locate2D (Locate2D (locate2D))
import Football.Player (Team)

xgValue :: Double -> Double
xgValue distance =
    min 0.99 $ 1.123*exp(-0.203*distance)+0.023

locationXG :: (Match m, Monad m, Locate2D l) => Team -> l -> m Double
locationXG team l = do
  attackingDirection' <- attackingDirection team
  let (x, y) = locate2D l
  pure $ case attackingDirection' of
    AttackingLeftToRight -> xgValue (sqrt ((105-x)**2.0 + (34-y) **2.0))
    AttackingRightToLeft -> xgValue (sqrt (x**2.0 + (34-y) **2.0))


