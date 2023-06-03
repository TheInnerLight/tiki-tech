{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Football.Understanding.Space where

import Voronoi.JCVoronoi (JCVPoly (polyIndex), jcvSites2, findPoly)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Match (Match (..), clampPitch, AttackingDirection (..), teamPlayers)
import Football.Types
import Data.Maybe (mapMaybe)
import Data.Map (Map, (!))
import Data.List (foldl', minimumBy, maximumBy, sortOn)
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..), HorizontalZone(..), HorizontalHalf (..), CentresOfPlay (centresOfPlayBothTeams, CentresOfPlay, centresOfPlayTeam1, centresOfPlayTeam2), CentresOfPlayCache)
import qualified Data.Vector as Vec
import Statistics.Quantile (median, medianUnbiased)
import Data.Ord (comparing, Down(..))
import Football.Pitch (Pitch(pitchLength, pitchWidth), pitchHalfwayLineX)
import Core (Cache, cached, CacheKeyValue (CacheKey, CacheValue))

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

calcCentresOfPlay :: (Match m, Monad m) => m CentresOfPlay
calcCentresOfPlay = do
  players' <- allPlayers
  let (xs, ys) = unzip $ locate2D <$> players'
  let (t1xs, t1ys) = unzip $ locate2D <$> filter (\p -> playerTeam p == Team1) players'
  let (t2xs, t2ys) = unzip $ locate2D <$> filter (\p -> playerTeam p == Team2) players'
  let allPlayersCofP = (median medianUnbiased $ Vec.fromList xs, median medianUnbiased $ Vec.fromList ys)
  let t1CofP = (median medianUnbiased $ Vec.fromList t1xs, median medianUnbiased $ Vec.fromList t1ys)
  let t2CofP = (median medianUnbiased $ Vec.fromList t2xs, median medianUnbiased $ Vec.fromList t2ys)
  pure CentresOfPlay
    { centresOfPlayBothTeams = allPlayersCofP
    , centresOfPlayTeam1 = t1CofP
    , centresOfPlayTeam2 = t2CofP
    }


centresOfPlay :: (Match m, Monad m, Cache m CentresOfPlayCache) => m CentresOfPlay
centresOfPlay = do
  cached (const calcCentresOfPlay) ()


offsideLine :: (Match m, Monad m) => Team -> m Double
offsideLine team  = do
  pitch' <- pitch
  teamPlayers' <- teamPlayers $ otherTeam team
  attackingDirection' <- attackingDirection team
  (ballX, _) <- locate2D <$> gameBall
  pure $ case attackingDirection' of
       AttackingLeftToRight -> min (pitchLength pitch')  $ max ballX                      $ max (pitchHalfwayLineX pitch') $ xPos (sortOn (Data.Ord.Down . xPos) teamPlayers' !! 1)
       AttackingRightToLeft -> min ballX                 $ min (pitchHalfwayLineX pitch') $ max 0                          $ xPos (sortOn xPos teamPlayers' !! 1                  ) 
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

  



