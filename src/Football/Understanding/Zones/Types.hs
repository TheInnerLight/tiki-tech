{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Football.Understanding.Zones.Types where

import Core (CacheKeyValue (CacheKey, CacheValue))
import Football.Types (Team, Player)
import Football.Understanding.Space.Data (SpacePoly)
import Data.Map (Map)

newtype ZoneMap = ZoneMap (Map Player SpacePoly)

data ZoneCache = ZoneCache

instance CacheKeyValue ZoneCache where
  type CacheKey ZoneCache = Team
  type CacheValue ZoneCache = ZoneMap
