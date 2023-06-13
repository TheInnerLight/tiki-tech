module Football.Understanding.Team where

import Control.Lens ((^.))
import Linear (V3(..), R1 (_x), R3 (_z), R2 (_y))
import Football.Match
import Football.Types
import Football.Pitch (Pitch(pitchLength, pitchWidth))

toTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> m (V3 Double)
toTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  pitch' <- pitch
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (pitchLength pitch' - coord ^. _x) (pitchWidth pitch' - coord ^. _y) (coord ^. _z)

fromTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> m (V3 Double)
fromTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  pitch' <- pitch
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (pitchLength pitch' - coord ^. _x) (pitchWidth pitch' - coord ^. _y) (coord ^. _z)

inTeamCoordinateSystem :: (Match m, Monad m) => Team -> V3 Double -> (V3 Double -> V3 Double) -> m (V3 Double)
inTeamCoordinateSystem team coord f = do
  x <- toTeamCoordinateSystem team coord
  fromTeamCoordinateSystem team (f x)
    