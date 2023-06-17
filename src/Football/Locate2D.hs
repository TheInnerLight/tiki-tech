{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Football.Locate2D where

import Football.Types
import Control.Lens ((^.))
import Linear (V2(..), V3(..), V4(..), _x, _y, _z, Metric (norm), normalize, R2 (_xy))

class Locate2D a where
  locate2D :: a -> V2 Double

instance Locate2D Player where
  locate2D p = 
    let ppv = playerPositionVector p
    in V2 (ppv ^. _x) (ppv ^. _y) 

instance Locate2D (Double, Double) where
  locate2D :: (Double, Double) -> V2 Double
  locate2D (x, y) = V2 x y

instance Locate2D (V2 Double) where
  locate2D :: V2 Double -> V2 Double
  locate2D = id

instance Locate2D Ball where
  locate2D p = 
    let bpv = ballPositionVector p
    in V2 (bpv ^. _x) (bpv ^. _y) 

instance Locate2D (V3 Double) where
  locate2D v = v ^. _xy

data ProjectFuture a = ProjectFuture Double a

instance Locate2D (ProjectFuture Player) where
  locate2D (ProjectFuture t p) = 
    let fppv = playerPositionVector p + playerMotionVector p * pure t
    in locate2D fppv



