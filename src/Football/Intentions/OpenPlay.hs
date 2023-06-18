{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.OpenPlay where

import Control.Lens ((^.))
import Football.Types
import Football.Understanding.DecisionFactors
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.Intentions.OnTheBall
import Football.Match
import Core (Log (logOutput), Cache, Random)
import Football.Behaviours.Marking (positionalOrientedZonalMark, playerMarkClosestOppositionPlayer, playerOrientedZonalMark)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Understanding.Interception.Data (InterceptionDataCache)
import Football.Understanding.Zones.Types (ZoneCache)
import Football.Behaviours.Press (coverShadowOfPlayerOrientedZonalMark)
import Football.Player (isGoalKeeper)
import Football.Understanding.Shape (outOfPossessionDesiredPosition)
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Pitch (ownGoalVector)
import Linear (R1(_x), R2 (_y), V3 (V3), Metric (norm), normalize)
import Football.GameTime (gameTimeAddSeconds)



decideOpenPlayIntention :: (Match m, Monad m, Log m, Random m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache, Cache m ZoneCache) => Player -> m Player
decideOpenPlayIntention player =
  if isGoalKeeper player then
    decideGoalKeeperOpenPlayIntention player
  else
    decideOutfieldOpenPlayIntention player
    
decideOutfieldOpenPlayIntention :: (Match m, Monad m, Log m, Random m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache, Cache m ZoneCache) => Player -> m Player
decideOutfieldOpenPlayIntention player = do
  time <- currentGameTime
  decisionFactors <- calculateDecisionFactors player
  newIntention <- case decisionFactors of
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.75) Nothing) player
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = False } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfClosestPlayerToBall = Just (ClosestPlayerToBall loc t), dfHasControlOfBall = False } -> do
      targetLoc <- clampPitch loc
      let timestep = max 0.1 $ min 0.5 (0.5*t)
      pure $ ControlBallIntention targetLoc $ gameTimeAddSeconds time timestep
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = DefensiveTransitionPhase } -> do
      loc <- coverShadowOfPlayerOrientedZonalMark player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = OutOfPossessionPhase } -> do
      loc <- playerOrientedZonalMark player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    DecisionFactors { dfHasControlOfBall = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = AttackingTransitionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = InPossessionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
    _  -> pure DoNothing
  pure player { playerIntention = newIntention }

decideGoalKeeperOpenPlayIntention :: (Match m, Monad m, Log m, Random m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache, Cache m ZoneCache) => Player -> m Player
decideGoalKeeperOpenPlayIntention player = do
  decisionFactors <- calculateDecisionFactors player
  time <- currentGameTime
  newIntention <- case decisionFactors of
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.75) Nothing) player
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = False } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfClosestPlayerToBall = Just (ClosestPlayerToBall loc t), dfHasControlOfBall = False } -> do
      targetLoc <- clampPitch loc
      let timestep = max 0.1 $ min 0.5 (0.5*t)
      pure $ ControlBallIntention targetLoc $ gameTimeAddSeconds time timestep
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = DefensiveTransitionPhase } -> do
      loc <- outOfPossessionDesiredPosition player
      pure $ RunToLocation loc $ gameTimeAddSeconds time 0.1
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = OutOfPossessionPhase } -> do
      ball <- gameBall
      loc <- outOfPossessionDesiredPosition player
      ogVev <- ownGoalVector (playerTeam player)
      let ballFromGoal = ballPositionVector ball - ogVev
          desiredDistanceFromGoal = norm  (V3 (loc ^. _x) (loc ^. _y) 0 - ogVev )
      let dest = locate2D $ 
            if norm ballFromGoal < desiredDistanceFromGoal then
              ogVev + ballFromGoal - (normalize ballFromGoal * 2.5)
            else 
              ogVev + (normalize ballFromGoal * pure desiredDistanceFromGoal)
      pure $ RunToLocation dest $ gameTimeAddSeconds time 0.1
    DecisionFactors { dfHasControlOfBall = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = AttackingTransitionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = InPossessionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      pure $ MoveIntoSpace targetLoc time
    _  -> pure DoNothing
  pure player { playerIntention = newIntention }  
