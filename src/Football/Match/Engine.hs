{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Football.Behaviours.Generic 
import Data.List (sortOn)
import Data.Maybe (isJust)
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passSafetyCoeff), PassTarget (PlayerTarget, SpaceTarget))
import Voronoi.JCVoronoi (JCVPoly, jcvSites2)
import Control.Concurrent.STM (readTMVar, writeTMVar)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Understanding.Space (createSpaceMap)
import Football.Understanding.Space.Data (SpaceMap)

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar [Player]
  , matchStateTeam1VoronoiMap :: TMVar [JCVPoly]
  , matchStateTeam2VoronoiMap :: TMVar [JCVPoly]
  , matchStateSpaceMap :: TMVar SpaceMap
  }

kickImpl :: (Monad m, Has m MatchState, LiftSTM m) => V3 Double -> m ()
kickImpl motionVector' = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  liftSTM $ do
    ball <- readTVar stBall
    let ball' = ball { ballMotionVector = ballMotionVector ball + motionVector'  }
    writeTVar stBall ball'

enactIntentions :: (Monad m, Has m MatchState, LiftSTM m, Match m, Log m, GetSystemTime m, Random m) => m ()
enactIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse enactIntention players
    liftSTM $ writeTVar (matchStatePlayers state) players'
  where 
    enactIntention player =
      case playerIntention player of
        KickIntention loc -> kickBallToLocation loc player
        MoveIntoSpace loc -> runTowardsLocation loc player
        ControlBallIntention -> controlBall player
        IntentionCooldown _ -> pure player
        DoNothing -> do stop player
      
canKickImpl :: (Monad m, Has m MatchState, LiftSTM m) => Player -> m Bool
canKickImpl player = do
    (state :: MatchState) <- has
    let stBall = matchStateBall state
    liftSTM $ do
      ball <- readTVar stBall
      let dist = norm (playerPositionVector player - ballPositionVector ball)
      pure (dist < 0.5) 

updateImpl :: (Monad m, Has m MatchState, LiftSTM m, Match m, Log m, GetSystemTime m, Random m) => Int -> m ()
updateImpl fps = do
  (state :: MatchState) <- has
  team1Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team1
  team2Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team2
  spaceMap <- createSpaceMap
  liftSTM $ do
    writeTMVar (matchStateTeam1VoronoiMap state) team1Voronoi
    writeTMVar (matchStateTeam2VoronoiMap state) team2Voronoi
    writeTMVar (matchStateSpaceMap state) spaceMap
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
      ball <- gameBall
      teamPlayers' <- teammates player
      safePassingOptions <- safestPassingOptions player
      nearbySpace <- optimalNearbySpace player
      logOutput safePassingOptions
      let closerPlayers = 
            sortOn (\p -> interceptionTimePlayerBall p ball)
            . filter (\p -> interceptionTimePlayerBall p ball < interceptionTimePlayerBall player ball)
            . filter (\p -> interceptionTimePlayerBall p ball < 1/0)
            $ teamPlayers'
          canReach = interceptionTimePlayerBall player ball < 1/0
          
          relBallSpeed = norm (ballMotionVector ball - playerMotionVector player)
          newIntention = case (closerPlayers, safePassingOptions) of
              (cps : _, _) | not (null nearbySpace) -> 
                MoveIntoSpace $ locate2D $ head nearbySpace
              (_, passOpt : _) | canReach && relBallSpeed <= 9 && passSafetyCoeff passOpt >= 0.7  ->
                case passTarget passOpt of
                  PlayerTarget targetPlayer ->
                    let tppv = playerPositionVector targetPlayer + 0.3*(playerMotionVector targetPlayer)
                    in KickIntention (tppv ^. _x, tppv ^. _y)
                  SpaceTarget targetLoc ->
                    KickIntention targetLoc
              _ | canReach ->
                ControlBallIntention
              _ -> 
                ControlBallIntention
      --logOutput $ player { playerIntention = newIntention }
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

  
