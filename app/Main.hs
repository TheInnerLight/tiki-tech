{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign.C.Types (CInt)
import qualified MyLib (someFunc)
import qualified SDL as S
import qualified SDL.Font as SDLFont
import qualified SDL.Framerate as SF
import qualified SDL.Primitive as SP
import qualified SDL.Video.Renderer as SVR
import SDL.Vect             (V2(..), V3(..), V4(..), _x, _y, _z, Point(..))
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
import Football.Types
import Data.Foldable (traverse_)
import Voronoi.JCVoronoi

import qualified Data.Map as Map
import Football.Understanding.Space.Data (SpaceMap(SpaceMap))
import RkTests (trySomeBalls)
import App
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader.Class (MonadReader(ask))
import qualified Data.Text as T
import qualified Text.Printf as TText
import Football.Events.Goal (score)
import Football.Understanding.Zones (getZoneMap)
import Football.Understanding.Zones.Types (ZoneMap(ZoneMap))
import Football.Understanding.Space (getSpaceMapForTeam, offsideLine)
import Football.Understanding.LineBreaking (oppositionLines)
import Football.Locate2D (Locate2D(locate2D))
import Football.MatchStats (passesCompleted, oppositionPassesPerDefensiveAction, possession, shots, pitchTilt, interceptions, tackles, corners)
import Football.Types (TeamId(TeamId1))
import Core (Log(logOutput))

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

processFps :: Int
processFps = 30

renderFPS :: Int
renderFPS = 60


main :: IO ()
main = do
  trySomeBalls
  S.initialize [S.InitVideo]
  SDLFont.initialize
  font <- SDLFont.load "Roboto-Regular.ttf" 18
  h2font <- SDLFont.load "Roboto-Regular.ttf" 32
  w <- S.createWindow "Tiki Tech" S.defaultWindow { S.windowInitialSize = V2 screenWidth screenHeight }
  r <- S.createRenderer w (-1) S.defaultRenderer
  S.showWindow w

  let fonts = Fonts {fontsH2 = h2font, fontsDefault = font}
  
  SF.with renderFPS $ loopFor r fonts

  S.destroyWindow w
  S.quit
  where
    screenWidth = 2300
    screenHeight = 1460

playerDefaultAcceleration :: Double
playerDefaultAcceleration = 1.3

playerDefaultMaxSpeed :: Double
playerDefaultMaxSpeed = 7.8

player :: PlayerState
player = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 1
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-50) 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing
  }

player2 :: PlayerState
player2 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 2
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-35) 20 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player3 :: PlayerState
player3 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 3
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-35) (-20) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player4 :: PlayerState
player4 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 4
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-35) 10 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player5 :: PlayerState
player5 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 5
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-35) (-10) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player6 :: PlayerState
player6 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 6
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-30.0) 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player8 :: PlayerState
player8 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 8
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-25) 5 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player10 :: PlayerState
player10 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 10
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-26) (-6) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }


player7 :: PlayerState
player7 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 7
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-15) 10 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player9 :: PlayerState
player9 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 9
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-15) 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player11 :: PlayerState
player11 = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 11
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId1
    }
  , playerStatePositionVector = V3 (-15) (-10) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player1B :: PlayerState
player1B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 1
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 50 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player2B :: PlayerState
player2B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 2
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 35 (-20) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player3B :: PlayerState
player3B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 3
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 35 20 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player4B :: PlayerState
player4B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 4
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 35 (-10) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player5B :: PlayerState
player5B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 5
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 35 (10) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player6B :: PlayerState
player6B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 6
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 30.0 0 0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player8B :: PlayerState
player8B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 8
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 25.0 (-5) 0.0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player7B :: PlayerState
player7B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 7
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 15 (-10) 0
  , playerStateMotionVector = V3 0.0 0.0 0.0 
  , playerStateIntention = DoNothing
  }

player9B :: PlayerState
player9B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 9
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 15.0 0 0.0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player10B :: PlayerState
player10B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 10
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 26.0 6 0.0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

player11B :: PlayerState
player11B = PlayerState
  { playerStatePlayer = Player
    { playerNumber = 11
    , playerSpeed = PlayerSpeed { playerSpeedAcceleration = playerDefaultAcceleration, playerSpeedMax = playerDefaultMaxSpeed }
    , playerTeamId = TeamId2
    }
  , playerStatePositionVector = V3 15.0 10.0 0.0
  , playerStateMotionVector = V3 0.0 0.0 0.0
  , playerStateIntention = DoNothing --KickIntention (15.0, 45.0)
  }

ball :: Ball
ball = Ball { ballPositionVector = V3 0 0 0, ballMotionVector = V3 0.0 0.0 0.0 }

processLoop :: Int -> AppM ()
processLoop desiredFps = do
  t1 <- liftIO getSystemTime
  let (n1 :: Integer) = fromIntegral (systemSeconds t1 * 1000000000) + fromIntegral (systemNanoseconds t1)
  update desiredFps
  t2 <- liftIO getSystemTime
  let (n2 :: Integer) = fromIntegral (systemSeconds t2 * 1000000000) + fromIntegral (systemNanoseconds t2)
      microDiff = fromIntegral (n2-n1) `div` 1000
      eDiff = max 0 (1000000 `div` desiredFps - microDiff)
  liftIO $ threadDelay eDiff
  processLoop desiredFps

data Fonts = Fonts
  { fontsH2:: SDLFont.Font
  , fontsDefault :: SDLFont.Font
  }


loopFor :: S.Renderer -> Fonts-> SF.Manager -> IO ()
loopFor r fonts fpsm = do
  let playerStates = [player, player2, player3, player4, player5, player6, player7, player8, player9, player10, player11, player1B, player2B, player3B, player4B, player5B, player6B, player7B, player8B, player9B, player10B, player11B]
  let playerMap = Map.fromList $ fmap (\p -> (playerStatePlayer p, p)) playerStates
  let liverbird = Team
        { teamName = "Liverbird"
        , teamFormation = Formation
          { formationLine1 = FourLine (playerStatePlayer player3) (playerStatePlayer player5) (playerStatePlayer player4) (playerStatePlayer player2)
          , formationLine2 = EmptyLine
          , formationLine3 = ThreeLine (playerStatePlayer player10) (playerStatePlayer player6) (playerStatePlayer player8)
          , formationLine4 = EmptyLine
          , formationLine5 = ThreeLine (playerStatePlayer player11) (playerStatePlayer player9) (playerStatePlayer player7)
          }
        }
  let manshippy = Team
        { teamName = "Man Shippy"
        , teamFormation = Formation
          { formationLine1 = FourLine (playerStatePlayer player3B) (playerStatePlayer player5B) (playerStatePlayer player4B) (playerStatePlayer player2B)
          , formationLine2 = EmptyLine
          , formationLine3 = ThreeLine (playerStatePlayer player10B) (playerStatePlayer player6B) (playerStatePlayer player8B)
          , formationLine4 = EmptyLine
          , formationLine5 = ThreeLine (playerStatePlayer player11B) (playerStatePlayer player9B) (playerStatePlayer player7B)
          }
        }

  pt <- newTVarIO playerMap
  bt <- newTVarIO ball
  eventLog <- newTVarIO []
  t1Voronoi <- newEmptyTMVarIO
  t2Voronoi <- newEmptyTMVarIO
  allVoronoi <- newEmptyTMVarIO
  lastPlayerTouchedBall <- newEmptyTMVarIO
  cOfP <- newEmptyTMVarIO
  icache <- newEmptyTMVarIO
  gametimer <- newTVarIO $ GameTime FirstHalf 0
  gamestate <- newTVarIO $ RestartState $ KickOff TeamId1
  zonecache <- newEmptyTMVarIO
  spacecache <- newEmptyTMVarIO
  let initialState = 
        MatchState 
          { matchStateBall = bt
          , matchStatePlayers = pt
          , matchStateTeam1VoronoiMap = t1Voronoi
          , matchStateTeam2VoronoiMap = t2Voronoi
          , matchStateSpaceMap = allVoronoi
          , matchStateLastPlayerTouchedBall = lastPlayerTouchedBall
          , matchPitch = Pitch 105 68
          , matchStateEventLog = eventLog
          , matchStateCentresOfPlay = cOfP
          , matchStateInterceptionCache = icache
          , matchStateGameTime = gametimer
          , matchStateGameState = gamestate
          , matchStateZoneCache = zonecache
          , matchStateSpaceCache = spacecache
          , matchStateTeams = Map.fromList [(TeamId1,liverbird),(TeamId2,manshippy)]
          }
  _ <- forkIO $ runAppM (processLoop processFps) initialState
  runAppM loop' initialState
  where
    loop' :: AppM ()
    loop' = do
      frames <- fromIntegral `fmap` SF.count fpsm
      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r

      -- draw the pitch
      pitch' <- pitch
      liftIO $ render r (fontsDefault fonts) pitch'

      -- draw the players
      players <- allPlayers
      traverse_ (liftIO . render r (fontsDefault fonts)) players

      -- draw the ball
      ball' <- gameBall
      liftIO $ render r (fontsDefault fonts) ball'

      -- draw space polygons
      -- (SpaceMap sitesAll) <- spaceMap
      -- traverse_ (liftIO . render r (fontsDefault fonts)) $ fmap snd $ Map.toList sitesAll


      -- draw team space polygons
      -- (SpaceMap t1Sites) <- getSpaceMapForTeam Team1
      -- traverse_ (liftIO . render r (fontsDefault fonts)) $ fmap snd $ Map.toList t1Sites

      -- (SpaceMap t2Sites) <- getSpaceMapForTeam Team2
      -- traverse_ (liftIO . render r (fontsDefault fonts)) $ fmap snd $ Map.toList t2Sites

      --draw zone polygons
      -- (ZoneMap sitesAll) <- getZoneMap TeamId1
      -- traverse_ (liftIO . render r (fontsDefault fonts)) $ fmap snd $ Map.toList sitesAll

      -- offside1X <- offsideLine TeamId1
      -- liftIO $ render r (fontsDefault fonts) (OffsideLine offside1X)
      -- offside2X <- offsideLine TeamId2
      -- liftIO $ render r (fontsDefault fonts) (OffsideLine offside2X)


      -- lines <- oppositionLines Team1 
      -- let lines' = fmap (\(l1, l2) -> DefLine l1 l2) lines
      -- traverse_ (liftIO . render r (fontsDefault fonts)) lines'

      -- draw the scores
      (lg, mg) <- score

      pos1 <- possession TeamId1
      shots1 <- shots TeamId1
      (pc1, pa1) <- passesCompleted TeamId1
      pitchTilt1 <- pitchTilt TeamId1
      ints1 <- interceptions TeamId1
      tacks1 <- tackles TeamId1
      oppppda1 <- oppositionPassesPerDefensiveAction TeamId1
      corners1 <- corners TeamId1
      pos2 <- possession TeamId2
      shots2 <- shots TeamId2
      (pc2, pa2) <- passesCompleted TeamId2
      pitchTilt2 <- pitchTilt TeamId2
      ints2 <- interceptions TeamId2
      tacks2 <- tackles TeamId2
      oppppda2 <- oppositionPassesPerDefensiveAction TeamId2
      corners2 <- corners TeamId2

      let board1 = StatsBoard TeamId1 pos1 shots1 pc1 pa1 pitchTilt1 ints1 tacks1 oppppda1 corners1
      let board2 = StatsBoard TeamId2 pos2 shots2 pc2 pa2 pitchTilt2 ints2 tacks2 oppppda2 corners2
      liftIO $ render r (fontsDefault fonts) board1
      liftIO $ render r (fontsDefault fonts) board2

      (GameTime _ time) <- currentGameTime
      let (mm, ss) = (time `quot` 1000000) `quotRem` 60

      surf <- SDLFont.solid (fontsH2 fonts) white (T.pack (TText.printf "%02d" mm) <> ":" <> T.pack (TText.printf "%02d" ss) <> " Liverbird " <> T.pack (show lg) <> " - " <> T.pack (show mg) <> " Man Shippy")
      timeAndScoreTexture <- liftIO $ SVR.createTextureFromSurface r surf
      surfDimensions <- liftIO $  SVR.surfaceDimensions surf
      SVR.freeSurface surf
      S.rendererDrawColor r $= black
      let targetRect = S.Rectangle (P $ S.V2 150 20) surfDimensions
      _ <- liftIO $ S.fillRect r (Just targetRect)
      SVR.copy r timeAndScoreTexture Nothing (Just targetRect)
      S.destroyTexture timeAndScoreTexture

      S.present r
      SF.delay_ fpsm
      loop'

