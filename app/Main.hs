{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO, newEmptyTMVarIO)
import Football.Match
import Football.Match.Engine
import Data.Foldable (traverse_)
import Voronoi.JCVoronoi

import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(SpaceMap))
import RkTests (trySomeBalls)
import App
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader.Class (MonadReader(ask))

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
fps = 30


main :: IO ()
main = do
  trySomeBalls
  S.initialize [S.InitVideo]
  w <- S.createWindow "sdl2-gfx-example" S.defaultWindow { S.windowInitialSize = V2 screenWidth screenHeight }
  r <- S.createRenderer w (-1) S.defaultRenderer
  S.showWindow w
  
  SF.with fps $ loopFor r

  S.destroyWindow w
  S.quit
  where
    screenWidth = 2300
    screenHeight = 1360

playerDefaultAcceleration :: Double
playerDefaultAcceleration = 1.3

player :: Player
player = Player 
  { playerPositionVector = V3 2.0 34.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player2 :: Player
player2 = Player 
  { playerPositionVector = V3 15.0 56.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player3 :: Player
player3 = Player 
  { playerPositionVector = V3 15.0 11.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player4 :: Player
player4 = Player 
  { playerPositionVector = V3 15.0 41.0 0
  , playerNumber = 4
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player5 :: Player
player5 = Player 
  { playerPositionVector = V3 15.0 26.0 0
  , playerNumber = 5
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player6 :: Player
player6 = Player 
  { playerPositionVector = V3 25.0 34.0 0
  , playerNumber = 6
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player7 :: Player
player7 = Player 
  { playerPositionVector = V3 35.0 24.0 0
  , playerNumber = 7
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player8 :: Player
player8 = Player 
  { playerPositionVector = V3 35.0 44.0 0
  , playerNumber = 8
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player9 :: Player
player9 = Player 
  { playerPositionVector = V3 50.0 24.0 0
  , playerNumber = 9
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player10 :: Player
player10 = Player 
  { playerPositionVector = V3 55.0 34.0 0
  , playerNumber = 10
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player11 :: Player
player11 = Player 
  { playerPositionVector = V3 50.0 44.0 0
  , playerNumber = 11
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team1
  , playerDesiredLocation = V3 0 0 0
  }

player1B :: Player
player1B = Player 
  { playerPositionVector = V3 103.0 34.0 0
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player2B :: Player
player2B = Player 
  { playerPositionVector = V3 90.0 11.0 0
  , playerNumber = 2
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player3B :: Player
player3B = Player 
  { playerPositionVector = V3 90.0 56.0 0
  , playerNumber = 3
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player4B :: Player
player4B = Player 
  { playerPositionVector = V3 90.0 26.0 0
  , playerNumber = 4
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player5B :: Player
player5B = Player 
  { playerPositionVector = V3 90.0 41.0 0
  , playerNumber = 5
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player6B :: Player
player6B = Player 
  { playerPositionVector = V3 80.0 34.0 0
  , playerNumber = 6
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player7B :: Player
player7B = Player 
  { playerPositionVector = V3 70.0 44.0 0
  , playerNumber = 7
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player8B :: Player
player8B = Player 
  { playerPositionVector = V3 70.0 24.0 0
  , playerNumber = 8
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player9B :: Player
player9B = Player 
  { playerPositionVector = V3 55.0 24.0 0
  , playerNumber = 9
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player10B :: Player
player10B = Player 
  { playerPositionVector = V3 50.0 34.0 0
  , playerNumber = 10
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }

player11B :: Player
player11B = Player 
  { playerPositionVector = V3 55.0 44.0 0
  , playerNumber = 11
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = 5 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = DoNothing --KickIntention (15.0, 45.0)
  , playerTeam = Team2
  , playerDesiredLocation = V3 0 0 0
  }



ball :: Ball
ball = Ball { ballPositionVector = V3 3.0 34.0 0, ballMotionVector = V3 0.0 0.0 0.0 }

processLoop :: Int -> AppM ()
processLoop desiredFps = do
  t1 <- liftIO getSystemTime
  let (n1 :: Integer) = fromIntegral (systemSeconds t1 * 1000000000) + fromIntegral (systemNanoseconds t1)
  update desiredFps
  t2 <- liftIO getSystemTime
  let (n2 :: Integer) = fromIntegral (systemSeconds t2 * 1000000000) + fromIntegral (systemNanoseconds t2)
      microDiff = fromIntegral (n2-n1) `div` 1000
      eDiff = max 0 (1000000 `div` fps - microDiff)

  liftIO $ threadDelay eDiff
  processLoop desiredFps



loopFor :: S.Renderer -> SF.Manager -> IO ()
loopFor r fpsm = do
  pt <- newTVarIO [player, player2, player3, player4, player5, player6, player7, player8, player9, player10, player11, player1B, player2B, player3B, player4B, player5B, player6B, player7B, player8B, player9B, player10B, player11B]
  bt <- newTVarIO ball
  t1Voronoi <- newEmptyTMVarIO
  t2Voronoi <- newEmptyTMVarIO
  allVoronoi <- newEmptyTMVarIO
  lastPlayerTouchedBall <- newEmptyTMVarIO
  let initialState = 
        MatchState 
          { matchStateBall = bt
          , matchStatePlayers = pt
          , matchStateTeam1VoronoiMap = t1Voronoi
          , matchStateTeam2VoronoiMap = t2Voronoi
          , matchStateSpaceMap = allVoronoi
          , matchStateLastPlayerTouchedBall = lastPlayerTouchedBall
          }
  _ <- forkIO $ runAppM (processLoop 30) initialState
  runAppM loop' initialState
  where
    loop' :: AppM ()
    loop' = do
      frames <- fromIntegral `fmap` SF.count fpsm
      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r

      --update fps

      players <- allPlayers
      --traverse_ updateIntention players
      traverse_ (liftIO . render r) players
      ball' <- gameBall

      --traverse_ (liftIO . render r) polys

      liftIO $ render r Pitch

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


