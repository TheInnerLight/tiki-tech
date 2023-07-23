module Football.Understanding.Team where

import Control.Lens ((^.))
import Linear (V3(..), R1 (_x), R3 (_z), R2 (_y), V2)
import Football.Match
import Football.Types
import Football.Locate2D (Locate2D(..))
import Football.Pitch (pitchHalfLengthX)

advancementCoeff :: (Monad m, Match m, Locate2D a) => TeamId -> a -> m Double
advancementCoeff teamId a = do
  let pos = locate2D a
  tpos <- toTeamCoordinateSystem2D teamId pos
  pitch' <- pitch
  pure $ (tpos ^. _x + pitchHalfLengthX pitch') / pitchLength pitch'

toTeamCoordinateSystem :: (Match m, Monad m) => TeamId -> V3 Double -> m (V3 Double)
toTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (- coord ^. _x) (- coord ^. _y) (coord ^. _z)

toTeamCoordinateSystem2D :: (Match m, Monad m) => TeamId -> V2 Double -> m (V2 Double)
toTeamCoordinateSystem2D team v = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure v
    AttackingRightToLeft -> pure (-v)

fromTeamCoordinateSystem :: (Match m, Monad m) => TeamId -> V3 Double -> m (V3 Double)
fromTeamCoordinateSystem team coord = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure coord
    AttackingRightToLeft -> pure $ V3 (- coord ^. _x) (- coord ^. _y) (coord ^. _z)

fromTeamCoordinateSystem2D :: (Match m, Monad m) => TeamId -> V2 Double -> m (V2 Double)
fromTeamCoordinateSystem2D team v = do
  attackingDirection' <- attackingDirection team
  case attackingDirection' of
    AttackingLeftToRight -> pure v
    AttackingRightToLeft -> pure (-v)

inTeamCoordinateSystem :: (Match m, Monad m) => TeamId -> V3 Double -> (V3 Double -> V3 Double) -> m (V3 Double)
inTeamCoordinateSystem team coord f = do
  x <- toTeamCoordinateSystem team coord
  fromTeamCoordinateSystem team (f x)
