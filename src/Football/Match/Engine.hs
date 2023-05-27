{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Football.Match.Engine where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Core
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad (when)
import Football.Match
import Football.Behaviours.Kick
import Data.List (sortOn, foldl', find)
import Data.Maybe (isJust)
import Football.Behaviours.FindSpace (optimalNearbySpace, nearestSpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passSafetyCoeff, passDesirabilityCoeff), PassTarget (PlayerTarget, SpaceTarget))
import Voronoi.JCVoronoi (JCVPoly, jcvSites2)
import Control.Concurrent.STM (readTMVar, writeTMVar)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Understanding.Space (createSpaceMap)
import Football.Understanding.Space.Data (SpaceMap)
import Football.Understanding.DecisionFactors
import Data.Foldable (foldlM)
import Football.Behaviours.Marking (playerMarkClosestOppositionPlayer, positionalOrientedZonalMark)
import Football.Intentions.OnTheBall (determineOnTheBallIntention, OnTheBallCriteria (OnTheBallCriteria))

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar [Player]
  , matchStateTeam1VoronoiMap :: TMVar [JCVPoly]
  , matchStateTeam2VoronoiMap :: TMVar [JCVPoly]
  , matchStateSpaceMap :: TMVar SpaceMap
  , matchStateLastPlayerTouchedBall :: TMVar Player
  }


attackingDirectionImpl :: (Monad m, Has m MatchState, LiftSTM m) => Team -> m AttackingDirection
attackingDirectionImpl team =
  case team of
    Team1 -> pure AttackingLeftToRight
    Team2 -> pure AttackingRightToLeft

kickImpl :: (Monad m, Has m MatchState, LiftSTM m) => Player -> V3 Double -> m Ball
kickImpl player motionVector' = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  let stLastPlayerBall = matchStateLastPlayerTouchedBall state
  liftSTM $ do
    ball <- readTVar stBall
    let ball' = ball { ballMotionVector = ballMotionVector ball + motionVector'  }
    writeTVar stBall ball'
    writeTMVar stLastPlayerBall player
    pure ball'

lastTouchOfBallImpl :: (Monad m, Has m MatchState, LiftSTM m) => m (Maybe Player)
lastTouchOfBallImpl = do
  (state :: MatchState) <- has
  let stLastPlayerBall = matchStateLastPlayerTouchedBall state
  liftSTM $ tryReadTMVar stLastPlayerBall

enactIntentions :: (Monad m, Has m MatchState, LiftSTM m, Match m, Log m, GetSystemTime m, Random m) => m ()
enactIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse enactIntention players
    liftSTM $ writeTVar (matchStatePlayers state) players'
  where 
    enactIntention player =
      case playerIntention player of
        DribbleIntention iceptloc kloc -> dribbleToLocation iceptloc kloc player
        KickIntention iceptloc mot -> kickBallWith iceptloc mot player
        MoveIntoSpace loc -> pure $ runTowardsLocation loc player
        ControlBallIntention loc -> controlBall loc player
        IntentionCooldown _ -> pure player
        DoNothing -> pure $ stopMoving player
      
updateImpl :: (Monad m, Has m MatchState, LiftSTM m, Match m, Log m, GetSystemTime m, Random m) => Int -> m ()
updateImpl fps = do
  (state :: MatchState) <- has
  team1Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team1
  team2Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team2
  spaceMap' <- createSpaceMap
  liftSTM $ do
    writeTMVar (matchStateTeam1VoronoiMap state) team1Voronoi
    writeTMVar (matchStateTeam2VoronoiMap state) team2Voronoi
    writeTMVar (matchStateSpaceMap state) spaceMap'
  ensureBallInPlay
  decideIntentions
  enactIntentions
  liftSTM $ do
    ball <- readTVar $ matchStateBall state
    let ball' = updateBall (fromIntegral fps) ball
    writeTVar (matchStateBall state) ball'
    players <- readTVar $ matchStatePlayers state
    let players' = fmap (updatePlayer (fromIntegral fps)) players
    writeTVar (matchStatePlayers state) players'

allPlayersImpl :: (Monad m, Has m MatchState, LiftSTM m) => m [Player]
allPlayersImpl = do 
  (state :: MatchState) <- has
  liftSTM $ readTVar $ matchStatePlayers state

gameBallImpl :: (Monad m, Has m MatchState, LiftSTM m) => m Ball
gameBallImpl = do 
  (state :: MatchState) <- has
  liftSTM $ readTVar $ matchStateBall state

ensureBallInPlay :: (Monad m, Has m MatchState, LiftSTM m) => m ()
ensureBallInPlay = do 
  (state :: MatchState) <- has
  liftSTM $ do 
    ball <- readTVar $ matchStateBall state
    let bpv = ballPositionVector ball
        ball' = 
          if (bpv ^. _x < 0.0 || bpv ^. _x > 105.0 || bpv ^. _y < 0.0 || bpv ^. _y > 68.0) then
            ball { ballPositionVector = V3 55 34 0 }
          else
            ball
    writeTVar (matchStateBall state) ball'

decideIntentions :: (Monad m, Has m MatchState, LiftSTM m, Match m, Log m, GetSystemTime m) => m ()
decideIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse updateIntention players
    liftSTM $ writeTVar (matchStatePlayers state) players'
  where
    updateIntention player = do
      time <- systemTimeNow
      case playerIntention player of
        IntentionCooldown endTime | time < endTime -> pure player
        _ -> decideIntention player
    decideIntention player = do
      decisionFactors <- calculateDecisionFactors player
      newIntention <- case decisionFactors of
        DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = True } -> do
          determineOnTheBallIntention (OnTheBallCriteria (Just 0.6) Nothing) player
        DecisionFactors { dfHasControlOfBall = True, dfIsUnderPressure = True, dfInCompressedSpace = False } -> do
          determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) (Just 0)) player
        DecisionFactors { dfClosestPlayerToBall = Just (ClosestPlayerToBall loc _), dfHasControlOfBall = False, dfOppositionInPossession = Nothing } -> do
          targetLoc <- clampPitch loc
          pure $ ControlBallIntention targetLoc
        DecisionFactors { dfClosestPlayerToBall = Just (ClosestPlayerToBall loc t), dfHasControlOfBall = False, dfOppositionInPossession = Just _  } | t <= 3.0 -> do
          targetLoc <- clampPitch loc
          pure $ ControlBallIntention targetLoc
        DecisionFactors { dfClosestPlayerToBall = _, dfHasControlOfBall = False, dfOppositionInPossession = Just (OppositionInPossession _)  } -> do
          loc <- positionalOrientedZonalMark player
          pure $ ControlBallIntention loc
        DecisionFactors { dfTeammateInPossession = Just _} -> do
          nearbySpace <- optimalNearbySpace player
          targetLoc <- clampPitch nearbySpace
          pure $ MoveIntoSpace targetLoc
        DecisionFactors { dfHasControlOfBall = True} -> do
          determineOnTheBallIntention (OnTheBallCriteria (Just 0.85) Nothing) player
        _  -> pure DoNothing
      pure player { playerIntention = newIntention }
    

team1VoronoiMapImpl :: (Monad m, Has m MatchState, LiftSTM m) => m [JCVPoly]
team1VoronoiMapImpl = do
  (state :: MatchState) <- has
  liftSTM $ readTMVar $ matchStateTeam1VoronoiMap state

team2VoronoiMapImpl :: (Monad m, Has m MatchState, LiftSTM m) => m [JCVPoly]
team2VoronoiMapImpl = do
  (state :: MatchState) <- has
  liftSTM $ readTMVar $ matchStateTeam2VoronoiMap state

allPlayersVoronoiMapImpl :: (Monad m, Has m MatchState, LiftSTM m) => m SpaceMap
allPlayersVoronoiMapImpl = do
  (state :: MatchState) <- has
  liftSTM $ readTMVar $ matchStateSpaceMap state

  
