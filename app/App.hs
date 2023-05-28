{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module App where

import Core
import Football.Match
import Football.Match.Engine
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO, newEmptyTMVarIO)
import Data.Time.Clock.System (getSystemTime)
import Data.Random.Normal (normalIO')

newtype AppM a = 
  AppM {unAppM :: ReaderT MatchState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has AppM MatchState where
  has = AppM ask

instance LiftSTM AppM where
  liftSTM = liftIO . atomically

instance Match AppM where
  attackingDirection = attackingDirectionImpl
  gameBall = gameBallImpl
  lastTouchOfBall = lastTouchOfBallImpl
  allPlayers = allPlayersImpl
  kickBall = kickImpl
  update = updateImpl
  spaceMap = allPlayersVoronoiMapImpl
  pitch = pitchImpl

instance Log AppM where
  logOutput stuff = liftIO $ print stuff

instance GetSystemTime AppM where
  systemTimeNow = liftIO getSystemTime

instance Random AppM where
  randomNormalMeanStd :: Double -> Double -> AppM Double
  randomNormalMeanStd mean std = liftIO $ normalIO' (mean, std)

runAppM = runReaderT . unAppM

