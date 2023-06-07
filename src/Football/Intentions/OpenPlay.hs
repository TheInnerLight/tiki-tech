{-# LANGUAGE FlexibleContexts #-}

module Football.Intentions.OpenPlay where

import Football.Types
import Football.Understanding.DecisionFactors
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.Intentions.OnTheBall
import Football.Match
import Core (GetSystemTime(systemTimeNow), Log (logOutput), Cache)
import Football.Behaviours.Marking (positionalOrientedZonalMark, playerMarkClosestOppositionPlayer)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Understanding.Space.Data (CentresOfPlayCache)
import Football.Understanding.Interception.Data (InterceptionDataCache)

decideOpenPlayIntention :: (Match m, Monad m, Log m, GetSystemTime m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache) => Player -> m Player
decideOpenPlayIntention player = do
  decisionFactors <- calculateDecisionFactors player
  newIntention <- case decisionFactors of
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.75) Nothing) player
    DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = False } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfClosestPlayerToBall = Just (ClosestPlayerToBall loc t), dfHasControlOfBall = False } -> do
      targetLoc <- clampPitch loc
      time <- systemTimeNow
      let timestep = max 0.1 $ min 0.5 (0.5*t)
      pure $ ControlBallIntention targetLoc $ time { systemNanoseconds = systemNanoseconds time + floor (timestep*100000000) }
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = DefensiveTransitionPhase } -> do
      loc <- playerMarkClosestOppositionPlayer player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfGamePhase = OutOfPossessionPhase } -> do
      loc <- positionalOrientedZonalMark player
      time <- systemTimeNow
      pure $ RunToLocation loc $ time { systemNanoseconds = systemNanoseconds time + 100000000 }
    DecisionFactors { dfHasControlOfBall = True } -> do
      determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = AttackingTransitionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
    DecisionFactors { dfHasControlOfBall = False, dfGamePhase = InPossessionPhase } -> do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch nearbySpace
      time <- systemTimeNow
      pure $ MoveIntoSpace targetLoc $ time { systemNanoseconds = systemNanoseconds time }
    _  -> pure DoNothing
  pure player { playerIntention = newIntention }  
