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
import Football.Engine (Engine(..), kickImpl, updateIntentionImpl, MatchState(..), canKickImpl)

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
  kickBall = kickImpl
  updateIntention = updateIntentionImpl
  canKick = canKickImpl



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

-- data MatchState = MatchState 
--   { matchStateBall :: Ball
--   , matchStatePlayer :: Player
--   }

player :: Player
player = Player 
  { playerPosition = (35.0, 35.0)
  , playerNumber = 1
  , playerSpeed = PlayerSpeed { playerSpeedAcceleration = 0.6, playerSpeedMax = 9 }
  , playerMotionVector = V3 0.0 0.0 0.0 
  , playerIntention = KickIntention (45.0, 45.0)
  }

ball :: Ball
ball = Ball { ballPosition = (3.0, 14.0), ballMotionVector = V3 10.0 0.0 0.0 }


loopFor :: S.Renderer -> SF.Manager -> IO ()
loopFor r fpsm = do
  pt <- newTVarIO player
  bt <- newTVarIO ball
  let initialState = MatchState { matchStateBall = bt, matchStatePlayer = pt }
  runReaderT (unAppM loop') initialState
  where
    loop' :: AppM ()
    loop' = do
      frames <- fromIntegral `fmap` SF.count fpsm
      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r
      st <- has

      (ball', player') <- liftSTM $ do
        ball <- readTVar $ matchStateBall st
        let ball' = updateBall (fromIntegral fps) ball
        writeTVar (matchStateBall st) ball'
        player <- readTVar $ matchStatePlayer st
        let player' = updatePlayer (fromIntegral fps) player
        writeTVar (matchStatePlayer st) player'
        pure (ball', player')

      updateIntention player'

      liftIO $ render r player'
      liftIO $ render r ball'

      S.present r

      SF.delay_ fpsm

      loop'




-- loopFor :: S.Renderer -> SF.Manager -> IO ()
-- loopFor r fpsm = void $ loop' $ MatchState { matchStateBall = ball, matchStatePlayer = player}
--   where
--     loop' :: MatchState -> IO MatchState
--     loop' st = do
--       -- How many frames have we drawn until now?
--       frames <- fromIntegral `fmap` SF.count fpsm
--       -- Clear the screen!
--       S.rendererDrawColor r $= black
--       S.clear r

--       let ball' = updateBall (fromIntegral fps) $ matchStateBall st
--       let player' = updatePlayer (fromIntegral fps) . updateIntention ball' $ matchStatePlayer st

--       render r player'
--       render r ball'

--       S.present r

--       SF.delay_ fpsm

--       loop' $ st { matchStateBall = ball', matchStatePlayer = player' }

