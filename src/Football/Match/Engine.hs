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
import Control.Monad (when, void)
import Football.Match
import Football.Behaviours.Kick
import Data.List (sortOn, foldl', find)
import Data.Maybe (isJust)
import Voronoi.JCVoronoi (JCVPoly, jcvSites2)
import Control.Concurrent.STM (readTMVar, writeTMVar)
import Football.Locate2D (Locate2D(locate2D), ProjectFuture (ProjectFuture))
import Football.Understanding.Space (createSpaceMap)
import Football.Understanding.Space.Data (SpaceMap, CentresOfPlayCache, CentresOfPlay, SpaceCache)
import Football.Understanding.DecisionFactors
import Data.Foldable (foldlM)
import Football.Behaviours.Marking (playerMarkClosestOppositionPlayer, positionalOrientedZonalMark)
import Football.Intentions.OnTheBall (determineOnTheBallIntention, OnTheBallCriteria (OnTheBallCriteria))
import Football.Understanding.ExpectedGoals (locationXG)
import Football.Types
import Football.Events.Goal (checkForGoal)
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Control.Concurrent (tryTakeMVar, modifyMVar, modifyMVar_)
import Football.Intentions.OpenPlay (decideOpenPlayIntention)
import Data.Map (Map)
import Football.Understanding.Interception.Data (InterceptionData, InterceptionDataCache)
import qualified Data.Map as Map
import Football.Events.OutOfPlay (checkForThrowIn, checkedCrossedGoalLine)
import Football.Intentions.ThrowIn (decideThrowInIntention)
import Football.Intentions.Corner (decideCornerIntention)
import Football.Intentions.GoalKick (decideGoalKickIntention)
import Football.Intentions.KickOff (decideKickOffIntention)
import Football.Understanding.Zones.Types (ZoneCache, ZoneMap)
import Football.GameTime (gameTimeAddSeconds)

data MatchState = MatchState 
  { matchStateBall :: TVar Ball
  , matchStatePlayers :: TVar (Map Player PlayerState)
  , matchStateTeams :: Map TeamId Team
  , matchStateTeam1VoronoiMap :: TMVar [JCVPoly]
  , matchStateTeam2VoronoiMap :: TMVar [JCVPoly]
  , matchStateSpaceMap :: TMVar SpaceMap
  , matchStateLastPlayerTouchedBall :: TMVar Player
  , matchStateCentresOfPlay :: TMVar CentresOfPlay
  , matchStateInterceptionCache :: TMVar (Map (PlayerState, Ball) [InterceptionData])
  , matchStateZoneCache :: TMVar (Map TeamId ZoneMap)
  , matchStateSpaceCache :: TMVar (Map (Maybe TeamId) SpaceMap)
  , matchPitch :: Pitch
  , matchStateGameTime :: TVar GameTime
  , matchStateEventLog :: TVar [MatchLogEntry]
  , matchStateGameState :: TVar GameState
  }

matchEventLogImpl :: (Monad m, Has m MatchState, Atomise m) => m [MatchLogEntry]
matchEventLogImpl = do
  st <- has
  atomise $ readTVar $ matchStateEventLog st

recordInMatchEventLogImpl :: (Monad m, Has m MatchState, Atomise m) => MatchLogEntry -> m ()
recordInMatchEventLogImpl event = do
  st <- has
  atomise $ modifyTVar (matchStateEventLog st) (event : )

getGameStateImpl :: (Monad m, Has m MatchState, Atomise m) => m GameState
getGameStateImpl = do
  st <- has
  atomise $ readTVar (matchStateGameState st)

setGameStateImpl :: (Monad m, Has m MatchState, Atomise m) => GameState -> m ()
setGameStateImpl gs = do
  st <- has
  atomise $ writeTVar (matchStateGameState st) gs

pitchImpl :: (Monad m, Has m MatchState) => m Pitch
pitchImpl = matchPitch <$> has

attackingDirectionImpl :: (Monad m, Has m MatchState) => TeamId -> m AttackingDirection
attackingDirectionImpl team =
  case team of
    TeamId1 -> pure AttackingLeftToRight
    TeamId2 -> pure AttackingRightToLeft

kickImpl :: (Monad m, Match m, Has m MatchState, Atomise m) => Player -> TypeOfTouch -> V3 Double -> V3 Double -> m Ball
kickImpl player typeOfTouch loc motionVector' = do
  (state :: MatchState) <- has
  time <- currentGameTime
  let stBall = matchStateBall state
  let stLastPlayerBall = matchStateLastPlayerTouchedBall state
  atomise $ do
    ball <- readTVar stBall
    let ball' = ball { ballPositionVector = loc, ballMotionVector = ballMotionVector ball + motionVector'  }
    let loc2d = locate2D loc
    writeTVar stBall ball'
    writeTMVar stLastPlayerBall player
    modifyTVar' (matchStateEventLog state) (TouchLogEntry (TouchOfBall player time typeOfTouch loc2d) :)
    pure ball'

setBallMotionParamsImpl :: (Monad m, Match m, Has m MatchState, Atomise m) => V3 Double -> V3 Double -> m Ball
setBallMotionParamsImpl ballPos ballMot = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  atomise $ do
    ball <- readTVar stBall
    let ball' = ball { ballPositionVector = ballPos, ballMotionVector = ballMot }
    writeTVar stBall ball'
    pure ball'

enactIntentions :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, Random m) => m ()
enactIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse enactIntention players
    let playerMap = Map.fromList $ fmap (\p -> (playerStatePlayer p, p)  ) players'
    atomise $ writeTVar (matchStatePlayers state) playerMap
  where 
    enactIntention playerState =
      case playerStateIntention playerState of
        DribbleIntention iceptloc kloc -> dribbleToLocation kloc playerState
        PassIntention target iceptloc mot t -> do
          tNow <- currentGameTime
          if tNow >= gameTimeAddSeconds t (-0.1) then
            kickBallWithMotion mot PassTouch playerState
          else do
            ball <- gameBall
            pure $ playerState { playerStateIntention = PassIntention target (locate2D ball) mot t}
        ThrowIntention _ iceptloc mot -> do 
            p <- kickBallWithMotion mot TakeThrowTouch playerState
            setGameState OpenPlayState
            pure p
        TakeCornerIntention _ iceptloc mot -> do 
            p <- kickBallWithMotion mot TakeCornerTouch playerState
            setGameState OpenPlayState
            pure p
        TakeGoalKickIntention _ iceptloc mot -> do 
            p <- kickBallWithMotion mot TakeGoalKickTouch playerState
            setGameState OpenPlayState
            pure p
        TakeKickOffIntention _ iceptloc mot -> do 
            p <- kickBallWithMotion mot TakeKickOffTouch playerState
            setGameState OpenPlayState
            pure p
        ShootIntention _ iceptloc mot -> kickBallWithMotion mot ShotTouch playerState
        MoveIntoSpace loc _ -> pure playerState
        RunToLocation loc _ -> pure playerState
        ControlBallIntention loc _ -> controlBall playerState
        InterceptBallIntention loc _ -> interceptBall playerState
        TackleIntention loc _ -> tackle playerState
        IntentionCooldown _ -> pure playerState
        DoNothing -> pure playerState
      
updateImpl :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, Random m, Concurrent m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache, Cache m ZoneCache, Cache m SpaceCache) => Int -> m ()
updateImpl fps = do
  (state :: MatchState) <- has
  team1Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers TeamId1
  team2Voronoi <- jcvSites2 . fmap locate2D <$> teamPlayers TeamId2
  spaceMap' <- createSpaceMap
  atomise $ do
    writeTMVar (matchStateTeam1VoronoiMap state) team1Voronoi
    writeTMVar (matchStateTeam2VoronoiMap state) team2Voronoi
    writeTMVar (matchStateSpaceMap state) spaceMap'
    _ <- tryTakeTMVar (matchStateCentresOfPlay state)
    _ <- tryTakeTMVar (matchStateInterceptionCache state)
    _ <- tryTakeTMVar (matchStateZoneCache state)
    _ <- tryTakeTMVar (matchStateSpaceCache state)
    _ <- modifyTVar' (matchStateGameTime state) (\(GameTime h t) -> GameTime h (t+timeStep))
    pure ()
  checkForThrowIn
  checkedCrossedGoalLine
  resetBall
  decideIntentions
  enactIntentions
  atomise $ do
    ball <- readTVar $ matchStateBall state
    let ball' = updateBall (fromIntegral fps) ball
    writeTVar (matchStateBall state) ball'
    players <- readTVar $ matchStatePlayers state
    let players' = fmap (timeStepPlayerState (fromIntegral fps)) players
    writeTVar (matchStatePlayers state) players'
  where
    timeStep = 1000000 `div` fps

resetBall :: (Match m, Monad m, Has m MatchState, Atomise m) => m ()
resetBall = do
  st <- getGameState
  case st of
    RestartState (ThrowIn _ loc)    -> void $ setBallMotionParamsImpl (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
    RestartState (CornerKick _ loc) -> void $ setBallMotionParamsImpl (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
    RestartState (GoalKick _ loc)   -> void $ setBallMotionParamsImpl (V3 (loc ^. _x) (loc ^. _y) 0) (V3 0 0 0)
    RestartState (KickOff _)        -> void $ setBallMotionParamsImpl (V3 0 0 0) (V3 0 0 0)
    OpenPlayState -> pure ()

allPlayersImpl :: (Monad m, Has m MatchState, Atomise m) => m [PlayerState]
allPlayersImpl = do 
  (state :: MatchState) <- has
  map' <- atomise $ readTVar $ matchStatePlayers state
  pure $ snd <$> Map.toList map'

getPlayerStateImpl :: (Monad m, Has m MatchState, Atomise m) => Player -> m PlayerState
getPlayerStateImpl player = do
  (state :: MatchState) <- has
  map' <- atomise $ readTVar $ matchStatePlayers state
  pure $ map' Map.! player

getTeamImpl :: (Monad m, Has m MatchState) => TeamId -> m Team
getTeamImpl tid = do
  (state :: MatchState) <- has
  pure $ matchStateTeams state Map.! tid

gameBallImpl :: (Monad m, Has m MatchState, Atomise m) => m Ball
gameBallImpl = do 
  (state :: MatchState) <- has
  atomise $ readTVar $ matchStateBall state

decideIntentions :: (Monad m, Has m MatchState, Atomise m, Match m, Log m, Random m, Concurrent m, Cache m CentresOfPlayCache, Cache m InterceptionDataCache, Cache m ZoneCache, Cache m SpaceCache) => m ()
decideIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- mapConcurrently updateIntention players
    let playerMap = Map.fromList $ fmap (\p -> (playerStatePlayer p, p)  ) players'
    atomise $ writeTVar (matchStatePlayers state) playerMap
  where
    updateIntention playerState = do
      time <- currentGameTime
      gameState <- getGameState
      let player = playerStatePlayer playerState
      newIntention <- case (intentionCooldown (playerStateIntention playerState), gameState) of
        (Just endTime, _) | time < endTime -> pure $ playerStateIntention playerState -- don't change the player intention during the cooldown
        (_, OpenPlayState) -> decideOpenPlayIntention player
        (_, RestartState (ThrowIn loc team)) -> decideThrowInIntention loc team player
        (_, RestartState (CornerKick loc team)) -> decideCornerIntention loc team player
        (_, RestartState (GoalKick loc team)) -> decideGoalKickIntention loc team player
        (_, RestartState (KickOff team)) -> decideKickOffIntention team player
      pure $ playerState { playerStateIntention = newIntention }

cacheLookupCentreOfPlayImpl :: (Monad m, Has m MatchState, Atomise m) => () -> m (Maybe CentresOfPlay)
cacheLookupCentreOfPlayImpl () = do
  (state :: MatchState) <- has
  atomise $ tryReadTMVar $ matchStateCentresOfPlay state

cacheInsertCentreOfPlayImpl :: (Monad m, Has m MatchState, Atomise m) => () -> CentresOfPlay -> m ()
cacheInsertCentreOfPlayImpl () v = do
  (state :: MatchState) <- has
  atomise $ writeTMVar (matchStateCentresOfPlay state) v

cacheLookupInterceptionDataImpl :: (Monad m, Has m MatchState, Atomise m) => (PlayerState, Ball) -> m (Maybe [InterceptionData])
cacheLookupInterceptionDataImpl (playerState, ball) = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateInterceptionCache state
  case cache of
    Just c  -> pure $ Map.lookup (playerState, ball) c
    Nothing -> pure Nothing

cacheInsertInterceptionDataImpl :: (Monad m, Has m MatchState, Atomise m) => (PlayerState, Ball) -> [InterceptionData] -> m ()
cacheInsertInterceptionDataImpl (playerState, ball) v = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateInterceptionCache state
  case cache of
    Just c -> atomise $ writeTMVar (matchStateInterceptionCache state) $ Map.insert (playerState, ball) v c 
    Nothing -> atomise $ writeTMVar (matchStateInterceptionCache state) (Map.fromList [((playerState, ball), v)])

cacheLookupZoneDataImpl :: (Monad m, Has m MatchState, Atomise m) => TeamId -> m (Maybe ZoneMap)
cacheLookupZoneDataImpl team = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateZoneCache state
  case cache of
    Just c  -> pure $ Map.lookup team c
    Nothing -> pure Nothing

cacheInsertZoneDataImpl :: (Monad m, Has m MatchState, Atomise m) => TeamId -> ZoneMap -> m ()
cacheInsertZoneDataImpl team v = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateZoneCache state
  case cache of
    Just c -> atomise $ writeTMVar (matchStateZoneCache state) $ Map.insert team v c 
    Nothing -> atomise $ writeTMVar (matchStateZoneCache state) (Map.fromList [(team, v)])

cacheLookupSpaceDataImpl :: (Monad m, Has m MatchState, Atomise m) => Maybe TeamId -> m (Maybe SpaceMap)
cacheLookupSpaceDataImpl team = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateSpaceCache state
  case cache of
    Just c  -> pure $ Map.lookup team c
    Nothing -> pure Nothing

cacheInsertSpaceDataImpl :: (Monad m, Has m MatchState, Atomise m) => Maybe TeamId -> SpaceMap -> m ()
cacheInsertSpaceDataImpl team v = do
  (state :: MatchState) <- has
  cache <- atomise $ tryReadTMVar $ matchStateSpaceCache state
  case cache of
    Just c -> atomise $ writeTMVar (matchStateSpaceCache state) $ Map.insert team v c 
    Nothing -> atomise $ writeTMVar (matchStateSpaceCache state) (Map.fromList [(team, v)])

gameTimeImpl :: (Monad m, Has m MatchState, Atomise m) => m GameTime
gameTimeImpl = do
  (state :: MatchState) <- has
  atomise $ readTVar $ matchStateGameTime state
  
