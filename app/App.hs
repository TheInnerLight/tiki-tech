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
import Football.Understanding.Space.Data (CentresOfPlayCache, SpaceCache)
import Football.Understanding.Interception.Data (InterceptionDataCache)
import System.Random (randomRIO)
import Football.Understanding.Zones.Types (ZoneCache)

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
  allPlayers = allPlayersImpl
  getPlayerState = getPlayerStateImpl
  getTeam = getTeamImpl
  kickBall = kickImpl
  update = updateImpl
  pitch = pitchImpl
  currentGameTime = gameTimeImpl
  matchEventLog = matchEventLogImpl
  recordInMatchEventLog = recordInMatchEventLogImpl
  getGameState = getGameStateImpl
  setGameState = setGameStateImpl
  setBallMotionParams = setBallMotionParamsImpl

instance Log AppM where
  logOutput stuff = liftIO $ print stuff
  logFile file line = liftIO $ appendFile file (line <> "\n")

instance GetSystemTime AppM where
  systemTimeNow = liftIO getSystemTime

instance Random AppM where
  randomNormalMeanStd mean std = liftIO $ normalIO' (mean, std)
  randomRange mind maxd = liftIO $ randomRIO (mind, maxd)

instance Concurrent AppM where
  mapConcurrently :: Traversable t => (a -> AppM b) -> t a -> AppM (t b)
  mapConcurrently f t = do
    st <- has
    liftIO $ Async.mapConcurrently (flip runAppM st . f) t

instance Cache AppM CentresOfPlayCache where
  cacheLookup = cacheLookupCentreOfPlayImpl
  cacheInsert = cacheInsertCentreOfPlayImpl

instance Cache AppM InterceptionDataCache where
  cacheLookup = cacheLookupInterceptionDataImpl
  cacheInsert = cacheInsertInterceptionDataImpl

instance Cache AppM ZoneCache where
  cacheLookup = cacheLookupZoneDataImpl
  cacheInsert = cacheInsertZoneDataImpl

instance Cache AppM SpaceCache where
  cacheLookup = cacheLookupSpaceDataImpl
  cacheInsert = cacheInsertSpaceDataImpl


runAppM :: AppM a -> MatchState -> IO a
runAppM = runReaderT . unAppM

