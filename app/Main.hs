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
import Control.Monad (void)
import Control.Lens ((^.))

black :: SP.Color
black = V4 0 0 0 255

white :: SP.Color
white = V4 255 255 255 255

fps :: Int
fps = 60

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

data MatchState = MatchState 
  { matchStateBall :: Ball
  , matchStatePlayer :: Player
  }

player :: Player
player = Player 
  { playerPosition = (15.0, 35.0)
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 4 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = KickIntention (45.0, 45.0)
  }

ball :: Ball
ball = Ball { ballPosition = (3.0, 14.0), ballMotionVector = V3 20.0 0.0 0.0 }

loopFor :: S.Renderer -> SF.Manager -> IO ()
loopFor r fpsm = void $ loop' $ MatchState { matchStateBall = ball, matchStatePlayer = player}
  where
    loop' :: MatchState -> IO MatchState
    loop' st = do
      -- How many frames have we drawn until now?
      frames <- fromIntegral `fmap` SF.count fpsm
      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r

      let ball' = updateBall (fromIntegral fps) $ matchStateBall st
      let player' = updatePlayer (fromIntegral fps) . updateIntention ball' $ matchStatePlayer st

      render r player'
      render r ball'

      S.present r

      SF.delay_ fpsm

      loop' $ st { matchStateBall = ball', matchStatePlayer = player' }

