module Football.Understanding.Space where

import Voronoi.JCVoronoi (JCVPoly (polyIndex), jcvSites2, findPoly)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Match (Match (..), clampPitch, AttackingDirection (..), teamPlayers)
import Football.Player (Player, Team (Team2, Team1))
import Data.Maybe (mapMaybe)
import Data.Map (Map, (!))
import Data.List (foldl', minimumBy, maximumBy, sortOn)
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..), HorizontalZone(..), HorizontalHalf (..))
import qualified Data.Vector as Vec
import Statistics.Quantile (median, medianUnbiased)
import Data.Ord (comparing, Down(..))

createSpaceMap :: (Match m, Monad m) => m SpaceMap
createSpaceMap = do
  players' <- allPlayers
  allPlayersVoronoi <- jcvSites2 <$> traverse (clampPitch . locate2D . ProjectFuture 0.3) players'
  let map1 = Map.fromList $ fmap (\v -> (polyIndex v, v)) allPlayersVoronoi
  let folder acc (i, p) =
        case Map.lookup i map1 of
          Just poly -> Map.insert i (SpacePoly poly p) acc
          Nothing -> acc
  let map2 = foldl' folder Map.empty $ zip [0..]  players'
  pure $ SpaceMap map2

pitchHorizontalZone :: (Match m, Monad m, Locate2D x) => Team -> x -> m HorizontalZone
pitchHorizontalZone team x = do
  let (_, y) = locate2D x
  attackingDirection' <- attackingDirection team
  pure $
    if y <= 13.84 then
      WingHZ $ lowY attackingDirection'
    else if y <= 24.84 then
      HalfSpaceHZ $ lowY attackingDirection'
    else if y <= 43.16 then
      CentreHZ
    else if y <= 54.16 then
      HalfSpaceHZ $ highY attackingDirection'
    else
      WingHZ $ highY attackingDirection'
  where 
    lowY AttackingLeftToRight = LeftHalf
    lowY AttackingRightToLeft = RightHalf
    highY AttackingLeftToRight = RightHalf
    highY AttackingRightToLeft = LeftHalf 

centreOfPlay :: (Match m, Monad m) => m (Double, Double)
centreOfPlay = do
  players' <- allPlayers
  let (xs, ys) = unzip $ locate2D <$> players'
  pure (median medianUnbiased $ Vec.fromList xs, median medianUnbiased $ Vec.fromList ys)

offsideLine :: (Match m, Monad m) => Team -> m Double
offsideLine team  = do
  teamPlayers' <- teamPlayers $ otherTeam team
  attackingDirection' <- attackingDirection team
  (ballX, _) <- locate2D <$> gameBall
  pure $ case attackingDirection' of
       AttackingLeftToRight -> min 105   $ max ballX $ max 52.5 $ xPos (sortOn (Data.Ord.Down . xPos) teamPlayers' !! 1)
       AttackingRightToLeft -> min ballX $ min 52.5  $ max 0    $ xPos (sortOn xPos teamPlayers' !! 1)
  where 
    otherTeam Team1 = Team2
    otherTeam Team2 = Team1
    xPos player = fst $ locate2D player

isOffide :: (Match m, Monad m, Locate2D x) => Team -> x -> m Bool
isOffide team x = do
  offsideLine' <- offsideLine team
  attackingDirection' <- attackingDirection team
  pure $ case attackingDirection' of
    AttackingLeftToRight -> fst (locate2D x) > offsideLine'
    AttackingRightToLeft -> fst (locate2D x) < offsideLine'

isOnside :: (Match m, Monad m, Locate2D x) => Team -> x -> m Bool
isOnside team x = not <$> isOffide team x

  


