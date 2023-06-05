{-# LANGUAGE DataKinds #-}
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
import Football.Behaviours.Pass (safestPassingOptions, PassDesirability (passTarget, passSafetyCoeff, passDesirabilityCoeff))
import Voronoi.JCVoronoi (JCVPoly, jcvSites2)
import Control.Concurrent.STM (readTMVar, writeTMVar)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Understanding.Space (createSpaceMap)
import Football.Understanding.Space.Data (SpaceMap, CentresOfPlayCache, CentresOfPlay)
import Football.Understanding.DecisionFactors
import Data.Foldable (foldlM)
import Football.Behaviours.Marking (playerMarkClosestOppositionPlayer, positionalOrientedZonalMark)
import Football.Intentions.OnTheBall (determineOnTheBallIntention, OnTheBallCriteria (OnTheBallCriteria))
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Pitch (Pitch)
import Football.Types
import Football.Events.Goal (checkForGoal)
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Control.Concurrent (tryTakeMVar)
import Football.Intentions.OpenPlay (decideOpenPlayIntention)
import Data.Map (Map)
import Football.Understanding.Interception.Data (InterceptionData, InterceptionDataCache)
import qualified Data.Map as Map

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar [Player]
  , matchStateGoals :: TVar [Goal]
  , matchStateTeam1VoronoiMap :: TMVar [JCVPoly]
  , matchStateTeam2VoronoiMap :: TMVar [JCVPoly]
  , matchStateSpaceMap :: TMVar SpaceMap
  , matchStateLastPlayerTouchedBall :: TMVar Player
  , matchStateCentresOfPlay :: TMVar CentresOfPlay
  , matchStateInterceptionCache :: TMVar (Map (Player, Ball) [InterceptionData])
  , matchPitch :: Pitch
  }

goalsImpl :: (Monad m, Has m MatchState, Atomise m) => m [Goal]
goalsImpl = do
  st <- has
  atomise $ readTVar $ matchStateGoals st

recordGoalImpl :: (Monad m, Has m MatchState, Atomise m) => Goal -> m ()
recordGoalImpl goal = do
  st <- has
  atomise $ modifyTVar (matchStateGoals st) (goal : )

pitchImpl :: (Monad m, Has m MatchState) => m Pitch
pitchImpl = matchPitch <$> has

attackingDirectionImpl :: (Monad m, Has m MatchState) => Team -> m AttackingDirection
attackingDirectionImpl team =
  case team of
    Team1 -> pure AttackingLeftToRight
    Team2 -> pure AttackingRightToLeft

kickImpl :: (Monad m, Has m MatchState, Atomise m) => Player -> V3 Double -> V3 Double -> m Ball
kickImpl player loc motionVector' = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  let stLastPlayerBall = matchStateLastPlayerTouchedBall state
  atomise $ do
    ball <- readTVar stBall
    let ball' = ball { ballPositionVector = loc, ballMotionVector = ballMotionVector ball + motionVector'  }
    writeTVar stBall ball'
    writeTMVar stLastPlayerBall player
    pure ball'

lastTouchOfBallImpl :: (Monad m, Has m MatchState, Atomise m) => m (Maybe Player)
lastTouchOfBallImpl = do
  (state :: MatchState) <- has
  let stLastPlayerBall = matchStateLastPlayerTouchedBall state
  atomise $ tryReadTMVar stLastPlayerBall

enactIntentions :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, GetSystemTime m, Random m) => m ()
enactIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse enactIntention players
    atomise $ writeTVar (matchStatePlayers state) players'
  where 
    enactIntention player =
      case playerIntention player of
        DribbleIntention iceptloc kloc -> dribbleToLocation iceptloc kloc player
        PassIntention _ iceptloc mot -> kickBallWith iceptloc mot player
        ShootIntention _ iceptloc mot -> kickBallWith iceptloc mot player
        MoveIntoSpace loc _ -> pure player
        RunToLocation loc _ -> pure player
        ControlBallIntention loc _ -> controlBall loc player
        IntentionCooldown _ -> pure player
        DoNothing -> pure player
      
updateImpl :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, GetSystemTime m, Random m, Concurrent m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache) => Int -> m ()
updateImpl fps = do
  (state :: MatchState) <- has
  team1Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team1
  team2Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers Team2
  spaceMap' <- createSpaceMap
  atomise $ do
    writeTMVar (matchStateTeam1VoronoiMap state) team1Voronoi
    writeTMVar (matchStateTeam2VoronoiMap state) team2Voronoi
    writeTMVar (matchStateSpaceMap state) spaceMap'
    _ <- tryTakeTMVar (matchStateCentresOfPlay state)
    _ <- tryTakeTMVar (matchStateInterceptionCache state)
    pure ()
  checkForGoal
  ensureBallInPlay
  decideIntentions
  enactIntentions
  atomise $ do
    ball <- readTVar $ matchStateBall state
    let ball' = updateBall (fromIntegral fps) ball
    writeTVar (matchStateBall state) ball'
    players <- readTVar $ matchStatePlayers state
    let players' = fmap (updatePlayer (fromIntegral fps)) players
    writeTVar (matchStatePlayers state) players'

allPlayersImpl :: (Monad m, Has m MatchState, Atomise m) => m [Player]
allPlayersImpl = do 
  (state :: MatchState) <- has
  atomise $ readTVar $ matchStatePlayers state

gameBallImpl :: (Monad m, Has m MatchState, Atomise m) => m Ball
gameBallImpl = do 
  (state :: MatchState) <- has
  atomise $ readTVar $ matchStateBall state

ensureBallInPlay :: (Monad m, Has m MatchState, Atomise m) => m ()
ensureBallInPlay = do 
  (state :: MatchState) <- has
  atomise $ do 
    ball <- readTVar $ matchStateBall state
    let bpv = ballPositionVector ball
        ball' = 
          if (bpv ^. _x < 0.0 || bpv ^. _x > 105.0 || bpv ^. _y < 0.0 || bpv ^. _y > 68.0) then
            ball { ballPositionVector = V3 55 34 0 }
          else
            ball
    writeTVar (matchStateBall state) ball'

decideIntentions :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, GetSystemTime m, Concurrent m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache) => m ()
decideIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    --players' <- traverse updateIntention players
    players' <- mapConcurrently updateIntention players
    atomise $ writeTVar (matchStatePlayers state) players'
  where
    updateIntention player = do
      time <- systemTimeNow
      case intentionCooldown (playerIntention player) of
        Just endTime | time < endTime -> pure player
        _ -> decideOpenPlayIntention player

cacheLookupCentreOfPlayImpl :: (Monad m, Has m MatchState, Atomise m) => () -> m (Maybe CentresOfPlay)
cacheLookupCentreOfPlayImpl () = do
  (state :: MatchState) <- has
  atomise $ tryReadTMVar $ matchStateCentresOfPlay state

cacheInsertCentreOfPlayImpl :: (Monad m, Has m MatchState, Atomise m) => () -> CentresOfPlay -> m ()
cacheInsertCentreOfPlayImpl () v = do
  (state :: MatchState) <- has
  atomise $ writeTMVar (matchStateCentresOfPlay state) v

cacheLookupInterceptionDataImpl :: (Monad m, Has m MatchState, Atomise m) => (Player, Ball) -> m (Maybe [InterceptionData])
cacheLookupInterceptionDataImpl (player, ball) = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateInterceptionCache state
  case cache of
    Just c  -> pure $ Map.lookup (player, ball) c
    Nothing -> pure Nothing

cacheInsertInterceptionDataImpl :: (Monad m, Has m MatchState, Atomise m) => (Player, Ball) -> [InterceptionData] -> m ()
cacheInsertInterceptionDataImpl (player, ball) v = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateInterceptionCache state
  case cache of
    Just c -> atomise $ writeTMVar (matchStateInterceptionCache state) $ Map.insert (player, ball) v c 
    Nothing -> atomise $ writeTMVar (matchStateInterceptionCache state) (Map.fromList [((player, ball), v)])

team1VoronoiMapImpl :: (Monad m, Has m MatchState, Atomise m) => m [JCVPoly]
team1VoronoiMapImpl = do
  (state :: MatchState) <- has
  atomise $ readTMVar $ matchStateTeam1VoronoiMap state

team2VoronoiMapImpl :: (Monad m, Has m MatchState, Atomise m) => m [JCVPoly]
team2VoronoiMapImpl = do
  (state :: MatchState) <- has
  atomise $ readTMVar $ matchStateTeam2VoronoiMap state

allPlayersVoronoiMapImpl :: (Monad m, Has m MatchState, Atomise m) => m SpaceMap
allPlayersVoronoiMapImpl = do
  (state :: MatchState) <- has
  atomise $ readTMVar $ matchStateSpaceMap state

  
