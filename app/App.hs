{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module App where

import Core
import Football.Match
import Football.Match.Engine
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO, newEmptyTMVarIO)
import Data.Time.Clock.System (getSystemTime)
import Data.Random.Normal (normalIO')
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Async as Async

newtype AppM a = 
  AppM {unAppM :: ReaderT MatchState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has AppM MatchState where
  has = AppM ask

instance Atomise AppM where
  atomise = liftIO . atomically

instance Match AppM where
  attackingDirection = attackingDirectionImpl
  gameBall = gameBallImpl
  lastTouchOfBall = lastTouchOfBallImpl
  allPlayers = allPlayersImpl
  kickBall = kickImpl
  update = updateImpl
  spaceMap = allPlayersVoronoiMapImpl
  pitch = pitchImpl
  goals = goalsImpl
  recordGoal = recordGoalImpl

instance Log AppM where
  logOutput stuff = liftIO $ print stuff

instance GetSystemTime AppM where
  systemTimeNow = liftIO getSystemTime

instance Random AppM where
  randomNormalMeanStd :: Double -> Double -> AppM Double
  randomNormalMeanStd mean std = liftIO $ normalIO' (mean, std)

instance Concurrent AppM where
  mapConcurrently :: Traversable t => (a -> AppM b) -> t a -> AppM (t b)
  mapConcurrently f t = do
    st <- has
    liftIO $ Async.mapConcurrently (flip runAppM st . f) t

instance Cache AppM "centre-of-play" where
  cacheLookup = cacheLookupCentreOfPlayImpl
  cacheInsert = cacheInsertCentreOfPlayImpl


runAppM :: AppM a -> MatchState -> IO a
runAppM = runReaderT . unAppM

