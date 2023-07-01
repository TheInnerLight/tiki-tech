{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Football.Understanding.Interception.Data where

import Linear (V3)
import Core 
import Football.Types (Ball, PlayerState)

data InterceptionData = InterceptionData
  { interceptionDataTime :: Double
  , interceptionDataDistance :: Double
  , interceptionDataBallLocation :: V3 Double
  , interceptionDataBallVector :: V3 Double
  } deriving (Eq, Ord, Show)

data InterceptionDataCache = InterceptionDataCache

instance CacheKeyValue InterceptionDataCache where
  type CacheKey InterceptionDataCache = (PlayerState, Ball)
  type CacheValue InterceptionDataCache = [InterceptionData]

