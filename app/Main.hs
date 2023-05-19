{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Foreign.C.Types (CInt)
import qualified MyLib (someFunc)
import qualified SDL as S
import qualified SDL.Framerate as SF
import qualified SDL.Primitive as SP
import SDL.Vect             (V2(..), V3(..), V4(..), _x, _y, _z)
import SDL                  (($=))
import Football.Player
import Render
import Football.Ball
import Core
import Control.Monad (void)
import Control.Lens ((^.))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO, newEmptyTMVarIO)
import Football.Match
import Football.Match.Engine
import Data.Foldable (traverse_)
import Voronoi.Fortune (voronoiPolygons)
import Voronoi.JCVoronoi
import Data.Time.Clock.System (getSystemTime)
import Data.Random.Normal (normalIO')
import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(SpaceMap))

black :: SP.Color
black = V4 0 0 0 255

white :: SP.Color
white = V4 255 255 255 255

red :: SP.Color
red = V4 255 0 0 255

orange :: SP.Color
orange = V4 255 165 0 255

blue :: SP.Color
blue = V4 0 0 255 255

purple :: SP.Color
purple = V4 255 0 255 255

grey :: SP.Color
grey = V4 155 155 155 255

fps :: Int
fps = 60

newtype AppM a = 
  AppM {unAppM :: ReaderT MatchState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has AppM MatchState where
  has = AppM ask

instance LiftSTM AppM where
  liftSTM = liftIO . atomically

instance Match AppM where
  gameBall = gameBallImpl
  allPlayers = allPlayersImpl
  kickBall = kickImpl
  canKick = canKickImpl
  update = updateImpl
  -- team1VoronoiMap = team1VoronoiMapImpl
  -- team2VoronoiMap = team2VoronoiMapImpl
  spaceMap = allPlayersVoronoiMapImpl

instance Log AppM where
  logOutput stuff = liftIO $ print stuff

instance GetSystemTime AppM where
  systemTimeNow = liftIO getSystemTime

instance Random AppM where
  randomNormalMeanStd :: Double -> Double -> AppM Double
  randomNormalMeanStd mean std = liftIO $ normalIO' (mean, std)

main :: IO ()
main = do

  S.initialize [S.InitVideo]
  w <- S.createWindow "sdl2-gfx-example" S.defaultWindow { S.windowInitialSize = V2 screenWidth screenHeight }
  r <- S.createRenderer w (-1) S.defaultRenderer
  S.showWindow w
  
  SF.with fps $ loopFor r

  S.destroyWindow w
  S.quit
  where
    screenWidth = 2100
    screenHeight = 1360


player :: Player
player = Player 
  { playerPositionVector = V3 2.0 12.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = KickIntention (85.0, 45.0)
  , playerTeam = Team1
  }

player2 :: Player
player2 = Player 
  { playerPositionVector = V3 55.0 50.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  }

player3 :: Player
player3 = Player 
  { playerPositionVector = V3 2.0 45.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  }

player4 :: Player
player4 = Player 
  { playerPositionVector = V3 4.0 35.0 0
  , playerNumber = 4
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  }

player1B :: Player
player1B = Player 
  { playerPositionVector = V3 45.0 20.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }

player2B :: Player
player2B = Player 
  { playerPositionVector = V3 10.0 55.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }

player3B :: Player
player3B = Player 
  { playerPositionVector = V3 34.0 42.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }

player4B :: Player
player4B = Player 
  { playerPositionVector = V3 24.0 31.0 0
  , playerNumber = 4
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }


ball :: Ball
ball = Ball { ballPositionVector = V3 3.0 14.0 0, ballMotionVector = V3 0.0 0.0 0.0 }


loopFor :: S.Renderer -> SF.Manager -> IO ()
loopFor r fpsm = do
  pt <- newTVarIO [player, player2, player3, player4, player1B, player2B, player3B, player4B]
  bt <- newTVarIO ball
  t1Voronoi <- newEmptyTMVarIO
  t2Voronoi <- newEmptyTMVarIO
  allVoronoi <- newEmptyTMVarIO
  let initialState = MatchState { matchStateBall = bt, matchStatePlayers = pt, matchStateTeam1VoronoiMap = t1Voronoi, matchStateTeam2VoronoiMap = t2Voronoi, matchStateSpaceMap = allVoronoi }
  runReaderT (unAppM loop') initialState
  where
    loop' :: AppM ()
    loop' = do
      frames <- fromIntegral `fmap` SF.count fpsm
      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r

      update fps

      players <- allPlayers
      --traverse_ updateIntention players
      traverse_ (liftIO . render r) players
      ball' <- gameBall

      --traverse_ (liftIO . render r) polys

      liftIO $ render r ball'

      --liftIO $ print "-------------------------------"
      --vd <- liftIO $ jcVoronoi points
      --sites <- liftIO $ jcvSites vd
      --let sites = jcvSites2 points
      -- sites1 <- team1VoronoiMap
      -- sites2 <- team2VoronoiMap
      (SpaceMap sitesAll) <- spaceMap

      -- traverse_ (liftIO . render r . ColouredVPoly red)  sites1
      -- traverse_ (liftIO . render r . ColouredVPoly blue) sites2
      traverse_ (liftIO . render r) $ fmap snd $ Map.toList sitesAll


      --liftIO $ print sites

      S.present r

      SF.delay_ fpsm

      loop'


