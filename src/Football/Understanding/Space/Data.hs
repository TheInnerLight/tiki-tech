{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Football.Understanding.Space.Data where
import Data.Map (Map)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Types
import Core (CacheKeyValue (CacheKey, CacheValue))
import Linear (V2)

newtype SpaceMap = SpaceMap (Map Int SpacePoly)

data SpacePoly = SpacePoly
  { spacePolyJCV :: JCVPoly
  , spacePolyPlayer :: Player
  } deriving Show

data HorizontalHalf
  = LeftHalf
  | RightHalf
  deriving (Eq, Ord, Show)

data HorizontalZone
  = CentreHZ
  | HalfSpaceHZ HorizontalHalf
  | WingHZ HorizontalHalf
  deriving (Eq, Ord, Show)

data CentresOfPlay = CentresOfPlay
  { centresOfPlayBothTeams :: V2 Double
  , centresOfPlayTeam1 :: V2 Double
  , centresOfPlayTeam2 :: V2 Double
  }

data CentresOfPlayCache = CentresOfPlayCache

instance CacheKeyValue CentresOfPlayCache where
  type CacheKey CentresOfPlayCache = ()
  type CacheValue CentresOfPlayCache = CentresOfPlay

