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
import Football.Behaviours.FindSpace (findSpace)

kickImpl :: (Monad m, Has m MatchState, LiftSTM m) => (Double, Double) -> m ()
kickImpl (targetX, targetY) = do
  (state :: MatchState) <- has
  let stBall = matchStateBall state
  liftSTM $ do
    ball <- readTVar stBall
    let targetVector = V3 targetX targetY 0
        ballDirection = normalize (targetVector - ballPositionVector ball)
        dist = norm (targetVector - ballPositionVector ball)
        ball' = ball { ballMotionVector = ballMotionVector ball + ballDirection * pure (min 31 $ dist ** 0.5 * 2.6) }
    writeTVar stBall ball'

enactIntentions :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m) => m ()
enactIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse enactIntention players
    liftSTM $ writeTVar (matchStatePlayers state) players'
  where 
    enactIntention player =
      case playerIntention player of
        KickIntention loc -> kickBallToLocation loc player
        DoNothing -> do 
          findSpace player
          --stop player
      
canKickImpl :: (Monad m, Has m MatchState, LiftSTM m) => Player -> m Bool
canKickImpl player = do
    (state :: MatchState) <- has
    let stBall = matchStateBall state
    liftSTM $ do
      ball <- readTVar stBall
      let dist = norm (playerPositionVector player - ballPositionVector ball)
      pure (dist < 0.5) 

updateImpl :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m) => Int -> m ()
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
    

decideIntentions :: (Monad m, Has m MatchState, LiftSTM m, Engine m, Log m) => m ()
decideIntentions = do
    (state :: MatchState) <- has
    players <- allPlayers
    players' <- traverse (decideIntention players) players
    liftSTM $ writeTVar (matchStatePlayers state) players'
  where 
    decideIntention players player = do
      ball <- gameBall
      let closerPlayers = 
            sortOn (\p -> interceptionTimePlayerBall p ball)
            . filter (\p -> interceptionTimePlayerBall p ball < interceptionTimePlayerBall player ball)
            . filter (\p -> isJust $ interceptionTimePlayerBall p ball)
            . filter (\p -> playerTeam p == playerTeam player) 
            $ players
          canReach = isJust $ interceptionTimePlayerBall player ball
          closeTeamPlayers = 
            sortOn (\p -> dist p ball) 
            . filter (\p -> playerTeam p == playerTeam player) 
            . filter (\p -> distpp p player > 0) 
            $ players
          newIntention = case closerPlayers of
              cps : _ -> DoNothing
              _ | canReach ->
                let targetPlayer = head closeTeamPlayers
                    tppv = playerPositionVector targetPlayer 
                in KickIntention (tppv ^. _x, tppv ^. _y)
              _ -> DoNothing
      --logOutput $ player { playerIntention = newIntention }
      pure player { playerIntention = newIntention }
    --dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2.0 + (y2 - y1) **2.0
    dist p b = norm (playerPositionVector p - ballPositionVector b)
    distpp p1 p2 = norm (playerPositionVector p1 - playerPositionVector p2)

  
