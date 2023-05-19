{-# LANGUAGE FlexibleInstances #-}

module Football.Locate2D where

import Football.Player (Player(..))
import Control.Lens ((^.))
import Linear (V2(..), V3(..), V4(..), _x, _y, _z)

class Locate2D a where
  locate2D :: a -> (Double, Double)

instance Locate2D Player where
  locate2D p = 
    let ppv = playerPositionVector p
    in (ppv ^. _x, ppv ^. _y) 

instance Locate2D (V3 Double) where
  locate2D v = 
    (v ^. _x, v ^. _y)

data ProjectFuture a = ProjectFuture Double a

instance Locate2D (ProjectFuture Player) where
  locate2D (ProjectFuture t p) = 
    let fppv = playerPositionVector p + playerMotionVector p * pure t
    in (fppv ^. _x, fppv ^. _y) 


