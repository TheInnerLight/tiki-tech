module Football.Understanding.Space.Data where
import Data.Map (Map)
import Voronoi.JCVoronoi (JCVPoly)
import Football.Player (Player)

newtype SpaceMap = SpaceMap (Map Int SpacePoly)

data SpacePoly = SpacePoly
  { spacePolyJCV :: JCVPoly
  , spacePolyPlayer :: Player
  }