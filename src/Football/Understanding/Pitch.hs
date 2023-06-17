module Football.Understanding.Pitch where
import Football.Types 
import Linear (V3)
import Football.Match
import Football.Match (AttackingDirection(AttackingRightToLeft))
import Football.Pitch (rightGoalLine, leftGoalLine)


ownGoalVector :: (Monad m, Match m) => Team -> m (V3 Double)
ownGoalVector team = do
  ad <- attackingDirection team
  pitch' <- pitch
  case ad of
    AttackingLeftToRight -> do
      let (lgMin, lgMax) = leftGoalLine pitch'
      pure $ (lgMin + lgMax) / 2
    AttackingRightToLeft -> do
      let (lgMin, lgMax) = rightGoalLine pitch'
      pure $ (lgMin + lgMax) / 2


