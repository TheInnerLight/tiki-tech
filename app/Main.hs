{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO)
import Football.Engine
import Football.Behaviours.General
import Data.Foldable (traverse_)
import Voronoi.Fortune (voronoiPolygons)

black :: SP.Color
black = V4 0 0 0 255

white :: SP.Color
white = V4 255 255 255 255

fps :: Int
fps = 60

newtype AppM a = 
  AppM {unAppM :: ReaderT MatchState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Has AppM MatchState where
  has = AppM ask

instance LiftSTM AppM where
  liftSTM = liftIO . atomically

instance Engine AppM where
  gameBall = gameBallImpl
  allPlayers = allPlayersImpl
  kickBall = kickImpl
  canKick = canKickImpl
  update = updateImpl

instance Log AppM where
  logOutput stuff = liftIO $ print stuff



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
    screenWidth = 1050
    screenHeight = 680


player :: Player
player = Player 
  { playerPositionVector = V3 2.0 12.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = KickIntention (85.0, 45.0)
  , playerTeam = Team1
  }

player2 :: Player
player2 = Player 
  { playerPositionVector = V3 55.0 50.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  }

player3 :: Player
player3 = Player 
  { playerPositionVector = V3 2.0 45.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  }

player4 :: Player
player4 = Player 
  { playerPositionVector = V3 45.0 20.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }

player5 :: Player
player5 = Player 
  { playerPositionVector = V3 10.0 55.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }

player6 :: Player
player6 = Player 
  { playerPositionVector = V3 34.0 42.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  }



ball :: Ball
ball = Ball { ballPositionVector = V3 3.0 14.0 0, ballMotionVector = V3 0.0 0.0 0.0 }


loopFor :: S.Renderer -> SF.Manager -> IO ()
loopFor r fpsm = do
  pt <- newTVarIO [player, player2, player3, player4, player5, player6]
  bt <- newTVarIO ball
  let initialState = MatchState { matchStateBall = bt, matchStatePlayers = pt }
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

      let playerPositionPoint p = 
            let ppv = playerPositionVector p
            in (ppv ^. _x, ppv ^. _y) 
      let team1Players = filter (\p -> playerTeam p /= Team1) players
      let points = fmap playerPositionPoint players
      let polys = voronoiPolygons ((0, 0),(105.0,68.0)) points

      traverse_ (liftIO . render r) polys

      liftIO $ render r ball'

      S.present r

      SF.delay_ fpsm

      loop'


