{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Football.Understanding.Space.Data where
import Data.Map (Map)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Types
import Core (CacheKeyValue (CacheKey, CacheValue))

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
  { centresOfPlayBothTeams :: (Double, Double)
  , centresOfPlayTeam1 :: (Double, Double)
  , centresOfPlayTeam2 :: (Double, Double)
  }

data CentresOfPlayCache = CentresOfPlayCache

instance CacheKeyValue CentresOfPlayCache where
  type CacheKey CentresOfPlayCache = ()
  type CacheValue CentresOfPlayCache = CentresOfPlay

