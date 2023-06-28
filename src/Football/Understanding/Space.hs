{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Football.Understanding.Space where

import Control.Lens ((^.))
import Voronoi.JCVoronoi (JCVPoly (polyIndex), jcvSites2, findPoly)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Match (Match (..), AttackingDirection (..), teamPlayers, clampPitch)
import Football.Types
import Data.Maybe (mapMaybe)
import Data.Map (Map, (!))
import Data.List (foldl', minimumBy, maximumBy, sortOn)
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(..), SpacePoly (..), HorizontalZone(..), HorizontalHalf (..), CentresOfPlay (centresOfPlayBothTeams, CentresOfPlay, centresOfPlayTeam1, centresOfPlayTeam2), CentresOfPlayCache, SpaceCache)
import qualified Data.Vector as Vec
import Statistics.Quantile (median, medianUnbiased)
import Data.Ord (comparing, Down(..))
import Football.Pitch (pitchHalfwayLineX, pitchHalfLengthX)
import Core (Cache, cached, CacheKeyValue (CacheKey, CacheValue))
import Football.Player (playerControlCentre)
import Linear.V3 (_x)
import Data.Function (on)
import Linear (V2(V2))

createSpaceMap :: (Match m, Monad m) => m SpaceMap
createSpaceMap = do
  players' <- allPlayers
  allPlayersVoronoi <- jcvSites2 <$> traverse (clampPitch . locate2D . playerControlCentre 0.1) players'
  let map1 = Map.fromList $ fmap (\v -> (polyIndex v, v)) allPlayersVoronoi
  let folder acc (i, p) =
        case Map.lookup i map1 of
          Just poly -> Map.insert i (SpacePoly poly p) acc
          Nothing -> acc
  let map2 = foldl' folder Map.empty $ zip [0..]  players'
  pure $ SpaceMap map2

createSpaceMapForTeam :: (Match m, Monad m) => Team -> m SpaceMap
createSpaceMapForTeam team = do
  players' <- teamPlayers team
  allPlayersVoronoi <- jcvSites2 <$> traverse (clampPitch . locate2D . playerControlCentre 0.1) players'
  let map1 = Map.fromList $ fmap (\v -> (polyIndex v, v)) allPlayersVoronoi
  let folder acc (i, p) =
        case Map.lookup i map1 of
          Just poly -> Map.insert i (SpacePoly poly p) acc
          Nothing -> acc
  let map2 = foldl' folder Map.empty $ zip [0..]  players'
  pure $ SpaceMap map2

createSpaceMapForMaybeTeam :: (Match m, Monad m) => Maybe Team -> m SpaceMap
createSpaceMapForMaybeTeam (Just team) = createSpaceMapForTeam team
createSpaceMapForMaybeTeam Nothing = createSpaceMap

getSpaceMap :: (Match m, Monad m, Cache m SpaceCache) => m SpaceMap
getSpaceMap = cached createSpaceMapForMaybeTeam Nothing

getSpaceMapForTeam :: (Match m, Monad m, Cache m SpaceCache) => Team -> m SpaceMap
getSpaceMapForTeam team = cached createSpaceMapForMaybeTeam $ Just team

pitchHorizontalZone :: (Match m, Monad m, Locate2D x) => Team -> x -> m HorizontalZone
pitchHorizontalZone team x = do
  let (V2 _ y) = locate2D x
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
  let (xs, ys) = unzip $ fmap (\(V2 x y) -> (x, y)) $ locate2D <$> players'
  let (t1xs, t1ys) = unzip $ fmap (\(V2 x y) -> (x, y))$ locate2D <$> filter (\p -> playerTeam p == Team1) players'
  let (t2xs, t2ys) = unzip $ fmap (\(V2 x y) -> (x, y))$ locate2D <$> filter (\p -> playerTeam p == Team2) players'
  let allPlayersCofP = V2 (median medianUnbiased $ Vec.fromList xs) (median medianUnbiased $ Vec.fromList ys)
  let t1CofP = V2 (median medianUnbiased $ Vec.fromList t1xs) (median medianUnbiased $ Vec.fromList t1ys)
  let t2CofP = V2 (median medianUnbiased $ Vec.fromList t2xs) (median medianUnbiased $ Vec.fromList t2ys)
  pure CentresOfPlay
    { centresOfPlayBothTeams = allPlayersCofP
    , centresOfPlayTeam1 = t1CofP
    , centresOfPlayTeam2 = t2CofP
    }

centresOfPlay :: (Match m, Monad m, Cache m CentresOfPlayCache) => m CentresOfPlay
centresOfPlay = do
  cached (const calcCentresOfPlay) ()

mostAdvancedPlayer :: (Match m, Monad m) => Team -> m Player
mostAdvancedPlayer team = do
  teamPlayers' <- teamPlayers team
  attackingDirection' <- attackingDirection team
  let px p = playerPositionVector p ^. _x
  pure $ case attackingDirection' of
    AttackingLeftToRight -> maximumBy (compare `on` px) teamPlayers'
    AttackingRightToLeft -> minimumBy (compare `on` px) teamPlayers'

offsideLine :: (Match m, Monad m) => Team -> m Double
offsideLine team  = do
  pitch' <- pitch
  teamPlayers' <- teamPlayers $ oppositionTeam team
  attackingDirection' <- attackingDirection team
  (V2 ballX _) <- locate2D <$> gameBall

  pure $ case attackingDirection' of
       AttackingLeftToRight -> min (pitchLength pitch')  $ max ballX                      $ max (pitchHalfwayLineX pitch')  $ xPos (sortOn (Data.Ord.Down . xPos) teamPlayers' !! 1)
       AttackingRightToLeft -> min ballX                 $ min 0                          $ max (- pitchHalfLengthX pitch') $ xPos (sortOn xPos teamPlayers' !! 1                  ) 
  where 
    xPos player = locate2D player ^. _x

isOffide :: (Match m, Monad m, Locate2D x) => Team -> x -> m Bool
isOffide team x = do
  offsideLine' <- offsideLine team
  attackingDirection' <- attackingDirection team
  pure $ case attackingDirection' of
    AttackingLeftToRight -> locate2D x ^. _x > offsideLine'
    AttackingRightToLeft -> locate2D x ^. _x < offsideLine'

isOnside :: (Match m, Monad m, Locate2D x) => Team -> x -> m Bool
isOnside team x = not <$> isOffide team x

isInOwnHalf :: (Match m, Monad m) => Player -> m Bool
isInOwnHalf player = do
  attackingDirection' <- attackingDirection (playerTeam player)
  pitch' <- pitch
  pure $ case attackingDirection' of
    AttackingLeftToRight -> locate2D player ^. _x < pitchHalfwayLineX pitch'
    AttackingRightToLeft -> locate2D player ^. _x > pitchHalfwayLineX pitch'



