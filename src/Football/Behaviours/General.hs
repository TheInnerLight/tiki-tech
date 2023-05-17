{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Behaviours.General where

import Linear.V3
import Control.Lens ((^.))
import Linear (Metric(norm, dot, quadrance), normalize)
import Football.Ball
import Football.Player
import Core
import Control.Concurrent.STM.TVar
import Control.Monad (when)
import Football.Engine
import Football.Behaviours.Kick
import Football.Behaviours.Generic 
import Data.List (sortOn)
import Data.Maybe (isJust)
import Football.Behaviours.FindSpace (findSpace, optimalNearbySpace)

kickImpl :: (Monad m, Has m MatchState, LiftSTM m) => V3 Double -> m ()
kickImpl motionVector' = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  liftSTM $ do
    ball <- readTVar stBall
    let ball' = ball { ballMotionVector = ballMotionVector ball + motionVector'  }
    writeTVar stBall ball'

enactIntentions :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m, GetSystemTime m, Random m) => m ()
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

updateImpl :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m, GetSystemTime m, Random m) => Int -> m ()
updateImpl fps = do
  (state :: MatchState) <- has
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
    

decideIntentions :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m, GetSystemTime m) => m ()
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
      oppositionPlayers' <- oppositionPlayers $ playerTeam player
      teamPlayers' <- teamPlayers $ playerTeam player
      let closerPlayers = 
            sortOn (\p -> interceptionTimePlayerBall p ball)
            . filter (\p -> interceptionTimePlayerBall p ball < interceptionTimePlayerBall player ball)
            . filter (\p -> interceptionTimePlayerBall p ball < 1/0)
            $ teamPlayers'
          canReach = interceptionTimePlayerBall player ball < 1/0
          closeTeamPlayers = 
            sortOn (\p -> dist p ball) 
            . filter (\p -> any (\p2 -> pointToLineDistance (playerPositionVector player, playerPositionVector p) (playerPositionVector p2) < norm (playerPositionVector p2 - playerPositionVector player)  ) oppositionPlayers'  )
            . filter (\p -> distpp p player > 0) 
            $ teamPlayers'
          relBallSpeed = norm (ballMotionVector ball - playerMotionVector player)
          nearbySpace = optimalNearbySpace player oppositionPlayers'
          newIntention = case closerPlayers of
              cps : _ | not (null nearbySpace) -> 
                MoveIntoSpace $ p2p $ head nearbySpace
              _ | canReach && relBallSpeed <= 9 && not (null closeTeamPlayers)  ->
                let targetPlayer = head closeTeamPlayers
                    tppv = playerPositionVector targetPlayer + playerMotionVector targetPlayer
                in KickIntention (tppv ^. _x, tppv ^. _y)
              _ | canReach ->
                ControlBallIntention
              _ -> 
                ControlBallIntention
      --logOutput $ player { playerIntention = newIntention }
      pure player { playerIntention = newIntention }
    --dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2.0 + (y2 - y1) **2.0
    dist p b = norm (playerPositionVector p - ballPositionVector b)
    distpp p1 p2 = norm (playerPositionVector p1 - playerPositionVector p2)
    p2p (V3 x y _) = (x, y)


pointToLineDistance :: (V3 Double, V3 Double) -> V3 Double -> Double
pointToLineDistance vs@(V3 x1 y1 z1, V3 x2 y2 z2) point =
  let normal = normalize $  V3 (x1 - x2) (y2 - y1) (z2-z1)
      (v1, _) = vs
      lineV = point - v1
  in abs $ dot lineV normal


  
