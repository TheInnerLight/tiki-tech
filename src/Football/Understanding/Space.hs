module Football.Understanding.Space where

import Voronoi.JCVoronoi (JCVPoly (polyIndex), jcvSites2, findPoly)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Match (Match (..), clampPitch)
import Football.Player (Player)
import Data.Maybe (mapMaybe)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..))
import GHC.IO (unsafePerformIO)

createSpaceMap :: (Match m, Monad m) => m SpaceMap
createSpaceMap = do
  players' <- allPlayers
  allPlayersVoronoi <- jcvSites2 <$> traverse (clampPitch . locate2D . ProjectFuture 0.3) players'
  let map1 = Map.fromList $ fmap (\v -> (polyIndex v, v)) allPlayersVoronoi
  let map2 = zipWith (\i p -> (i, SpacePoly (map1 ! i) p)) [0..] players'
  pure . SpaceMap $ Map.fromList map2

