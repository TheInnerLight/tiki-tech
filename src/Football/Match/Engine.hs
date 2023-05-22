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
import Football.Behaviours.FindSpace (optimalNearbySpace)
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passSafetyCoeff), PassTarget (PlayerTarget, SpaceTarget))
import Voronoi.JCVoronoi (JCVPoly, jcvSites2)
import Control.Concurrent.STM (readTMVar, writeTMVar)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Understanding.Space (createSpaceMap)
import Football.Understanding.Space.Data (SpaceMap)
import Football.Understanding.DecisionFactors
import Data.Foldable (foldlM)

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar [Player]
  , matchStateTeam1VoronoiMap :: TMVar [JCVPoly]
  , matchStateTeam2VoronoiMap :: TMVar [JCVPoly]
  , matchStateSpaceMap :: TMVar SpaceMap
  , matchStateLastPlayerTouchedBall :: TMVar Player
  }

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
        KickIntention iceptloc kloc -> kickBallToLocation iceptloc kloc player
        MoveIntoSpace loc -> pure $ runTowardsLocation loc player
        ControlBallIntention loc -> controlBall loc player
        IntentionCooldown _ -> pure player
        DoNothing -> pure $ stopMoving player
      
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
    folder _ acc (ClosestPlayerToBall loc t)   = do
      targetLoc <- clampPitch loc
      pure $ ControlBallIntention targetLoc : acc
    folder player acc (TeammateInPossession _) = do
      nearbySpace <- optimalNearbySpace player
      targetLoc <- clampPitch $ locate2D $ head nearbySpace
      pure $ MoveIntoSpace targetLoc : acc
    folder player acc HasControlOfBall = do
      ball <- gameBall
      safePassingOptions <- safestPassingOptions player
      case find (\p -> passSafetyCoeff p >= 0.7) safePassingOptions of
        Just pass -> 
          case passTarget pass of
            PlayerTarget targetPlayer ->
              let tppv = playerPositionVector targetPlayer + 0.3 * (playerMotionVector targetPlayer)
              in pure $ KickIntention (locate2D ball) (tppv ^. _x, tppv ^. _y) : acc
            SpaceTarget targetLoc ->
              pure $ KickIntention (locate2D ball) targetLoc : acc
        Nothing -> pure acc
    updateIntention player = do
      time <- systemTimeNow
      case playerIntention player of
        IntentionCooldown endTime | time < endTime -> pure player
        _ -> decideIntention player
    decideIntention player = do
      decisionFactors <- calculateDecisionFactors player
      newIntention <- head <$> foldlM (folder player) [DoNothing] decisionFactors
      -- ball <- gameBall
      -- teamPlayers' <- teammates player
      -- safePassingOptions <- safestPassingOptions player
      -- nearbySpace <- optimalNearbySpace player
      -- let (iceptLoc3D, iceptTime) = interceptionInfoPlayerBallRK player ball
      -- let iceptLoc = locate2D iceptLoc3D
      -- let closerPlayers = interceptionTimePlayersBallRK teamPlayers' ball < iceptTime -- filter (\p -> interceptionTimePlayerBallRK p ball < iceptTime) $ teamPlayers'
      --     canReach = iceptTime < 8
          
      --     relBallSpeed = norm (ballMotionVector ball - playerMotionVector player)
      --     newIntention = case (closerPlayers, safePassingOptions) of
      --         (True, _) | not (null nearbySpace) -> 
      --           MoveIntoSpace $ locate2D $ head nearbySpace
      --         (_, passOpt : _) | canReach && relBallSpeed <= 5 && passSafetyCoeff passOpt >= 0.7  ->
      --           case passTarget passOpt of
      --             PlayerTarget targetPlayer ->
      --               let tppv = playerPositionVector targetPlayer + 0.3*(playerMotionVector targetPlayer)
      --               in KickIntention iceptLoc (tppv ^. _x, tppv ^. _y)
      --             SpaceTarget targetLoc ->
      --               KickIntention iceptLoc targetLoc
      --         _ | canReach ->
      --           ControlBallIntention iceptLoc
      --         _ -> 
      --           DoNothing
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

  
