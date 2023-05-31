module Football.Understanding.Space.Data where
import Data.Map (Map)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Types

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
