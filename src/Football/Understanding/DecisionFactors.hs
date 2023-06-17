{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Football.Understanding.DecisionFactors where

import Football.Match
import Football.Locate2D (Locate2D(locate2D))
import Control.Monad (filterM, join)
import Linear (Metric(norm, distance), V2)
import Data.Foldable (find, Foldable (foldMap'))
import Data.Maybe (maybe, isJust, listToMaybe, catMaybes)
import Data.List (sort)
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(SpaceMap), SpacePoly (spacePolyJCV, spacePolyPlayer))
import qualified Data.Foldable as Map
import Voronoi.JCVoronoi (voronoiPolygonArea)
import Football.Behaviours.FindSpace (findClosestOpposition)
import Football.Understanding.Interception.Data
import Football.Understanding.Interception (interceptionInfoPlayerBallRK, interceptionTimePlayersBallRK, interceptionInfoPlayerBallRKI, safestInterceptionOption)
import Core (Log (logOutput), Cache)
import Football.Types
import Football.Behaviours.Kick (canKick)
import Football.Events (touchEvents, turnovers)
import Football.GameTime (gameTimeSeconds)

data DecisionFactors = DecisionFactors
  { dfClosestPlayerToBall :: Maybe ClosestPlayerToBall
  , dfGamePhase :: PhaseOfPlay
  , dfHasControlOfBall :: Bool
  , dfInCompressedSpace :: Bool
  , dfIsUnderPressure :: Bool
  } deriving Show

data ClosestPlayerToBall = ClosestPlayerToBall
  { closestPlayerToBallInterceptionLocation :: V2 Double
  , closestPlayerToBallInterceptionTime :: Double
  } deriving Show


checkClosestPlayer :: (Match m, Monad m, Log m, Cache m InterceptionDataCache) => Player -> m (Maybe ClosestPlayerToBall)
checkClosestPlayer player = do
  ball <- gameBall
  teamPlayers' <- teammates player
  interceptionOptions <- interceptionInfoPlayerBallRKI player ball
  case safestInterceptionOption interceptionOptions of
    Just iceptLoc -> do
      let iceptLoc2D = locate2D (interceptionDataBallLocation iceptLoc)
      otherIceptTime <- interceptionTimePlayersBallRK False teamPlayers' ball
      if otherIceptTime >= interceptionDataTime iceptLoc then
        pure $ Just $ ClosestPlayerToBall iceptLoc2D  (interceptionDataTime iceptLoc)
      else
        pure Nothing
    Nothing -> pure Nothing

checkPhase :: (Match m, Monad m, Log m) => Player -> m PhaseOfPlay
checkPhase player = do
  time <- currentGameTime
  maybeEvent <- listToMaybe <$> turnovers
  case maybeEvent of
    Just (TouchOfBall touchPlayer touchTime) | playerTeam touchPlayer == playerTeam player && gameTimeSeconds time - gameTimeSeconds touchTime <= 3 -> 
      pure AttackingTransitionPhase
    Just (TouchOfBall touchPlayer _) | playerTeam touchPlayer == playerTeam player ->
      pure InPossessionPhase
    Just (TouchOfBall _ touchTime) | gameTimeSeconds time - gameTimeSeconds touchTime <= 3 ->
      pure DefensiveTransitionPhase
    Just (TouchOfBall _ _) ->
      pure OutOfPossessionPhase
    Nothing -> 
      pure InPossessionPhase

checkInPossession :: (Match m, Monad m, Log m) => Player -> m Bool
checkInPossession player = do
  ball <- gameBall
  canKick' <- isJust <$> canKick player
  pure (canKick' && norm (playerMotionVector player - ballMotionVector ball) < 4)

checkInCompressedSpace :: (Match m, Monad m) => Player -> m Bool
checkInCompressedSpace player = do
  (SpaceMap spaceMap') <- spaceMap
  pure $ case Map.find (\poly -> spacePolyPlayer poly == player) spaceMap' of
    Just p -> voronoiPolygonArea (spacePolyJCV p) <= 5
    Nothing -> False

checkIsUnderPressure :: (Match m, Monad m) => Player -> m Bool
checkIsUnderPressure player = do
  op <- findClosestOpposition player
  pure $ distance (playerPositionVector player) (playerPositionVector op) <= 3.0

calculateDecisionFactors :: (Match m, Monad m, Log m, Cache m InterceptionDataCache) => Player -> m DecisionFactors
calculateDecisionFactors player = do
  cp <- checkClosestPlayer player
  hcb <- checkInPossession player
  comp <- checkInCompressedSpace player
  unp <- checkIsUnderPressure player
  phase <- checkPhase player
  pure $ DecisionFactors 
    { dfClosestPlayerToBall = cp
    , dfHasControlOfBall = hcb
    , dfInCompressedSpace = comp
    , dfIsUnderPressure = unp
    , dfGamePhase = phase
    }
