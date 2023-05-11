{-# LANGUAGE InstanceSigs #-}
module Render  
  ( Render(..)

  ) where

import qualified SDL as S
import qualified SDL.Primitive as SP
import SDL.Vect             (V2(..), V4(..))
import Football.Player
import Football.Ball

white :: SP.Color
white = V4 255 255 255 255

grey :: SP.Color
grey = V4 155 155 155 255

class Render a where
  render :: S.Renderer -> a -> IO ()

instance Render Player where
  render :: S.Renderer -> Player -> IO ()
  render r p = do
    let (px, py) = playerPosition p
        spx = floor $ px * 10.0
        spy = floor $ py * 10.0
    SP.fillCircle r (V2 spx spy) 5 white

instance Render Ball where
  render :: S.Renderer -> Ball -> IO ()
  render r b = do
    let (px, py) = ballPosition b
        spx = floor $ px * 10.0
        spy = floor $ py * 10.0
    SP.fillCircle r (V2 spx spy) 2 grey


