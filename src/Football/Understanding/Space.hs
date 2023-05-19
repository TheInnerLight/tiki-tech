module Football.Understanding.Space where

import Voronoi.JCVoronoi (JCVPoly (polyIndex), jcvSites2, findPoly)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Match (Match (..))
import Football.Player (Player)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..))

createSpaceMap :: (Match m, Monad m) => m SpaceMap
createSpaceMap = do
  players' <- allPlayers
  allPlayersVoronoi <- jcvSites2 . fmap (locate2D . ProjectFuture 0.3) <$> allPlayers
  let zipper:: Player -> (Int, SpacePoly)
      zipper player =
        let poly = fromJust $ findPoly allPlayersVoronoi . locate2D $ ProjectFuture 0.3 player
        in (polyIndex poly, SpacePoly poly player)
      stuff = fmap zipper players'
  pure . SpaceMap $ Map.fromList stuff

