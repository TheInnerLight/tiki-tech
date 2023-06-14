module Football.Understanding.Team where

import Control.Lens ((^.))
import Linear (V3(..), R1 (_x), R3 (_z), R2 (_y))
import Football.Match
import Football.Types
import Football.Pitch (Pitch(pitchLength, pitchWidth))

toTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> m (V3 Double)
toTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (- coord ^. _x) (- coord ^. _y) (coord ^. _z)

toTeamCoordinateSystem2D :: (Match m, Monad m) => Team -> (Double, Double) -> m (Double, Double)
toTeamCoordinateSystem2D team (x, y) = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure (x, y)
    AttackingRightToLeft -> pure (-x, -y)

fromTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> m (V3 Double)
fromTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (- coord ^. _x) (- coord ^. _y) (coord ^. _z)

fromTeamCoordinateSystem2D :: (Match m, Monad m) => Team -> (Double, Double) -> m (Double, Double)
fromTeamCoordinateSystem2D team (x, y) = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure (x, y)
    AttackingRightToLeft -> pure (-x, -y)

inTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> (V3 Double -> V3 Double) -> m (V3 Double)
inTeamCoordinateSystem team coord f = do
  x <- toTeamCoordinateSystem team coord
  fromTeamCoordinateSystem team (f x)
