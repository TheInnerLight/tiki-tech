{-# LANGUAGE InstanceSigs #-}
module Render  
  ( Render(..)

  ) where

import qualified SDL as S
import qualified SDL.Primitive as SP
import SDL.Vect             (V2(..), V4(..))
import Football.Player
import Football.Ball
import Control.Lens ((^.))
import Linear.V3 (_xy)
import Voronoi.Fortune (VoronoiPolygon(..))
import qualified Data.Vector.Storable as V

white :: SP.Color
white = V4 255 255 255 255

red :: SP.Color
red = V4 255 0 0 255

blue :: SP.Color
blue = V4 0 0 255 255

grey :: SP.Color
grey = V4 155 155 155 255

class Render a where
  render :: S.Renderer -> a -> IO ()

instance Render Player where
  render :: S.Renderer -> Player -> IO ()
  render r p = do
    let ppv = playerPositionVector p
        scaled = (ppv ^. _xy ) * 10
        scaled' = fmap floor scaled
        colour = 
          case playerTeam p of
            Team1 -> red
            Team2 -> blue 
    SP.fillCircle r scaled' 5 colour

instance Render Ball where
  render :: S.Renderer -> Ball -> IO ()
  render r b = do
    let bpv = ballPositionVector b
        scaled = (bpv ^. _xy ) * 10
        scaled' = fmap floor scaled
    SP.fillCircle r scaled' 2 grey

instance Render VoronoiPolygon where
  render :: S.Renderer -> VoronoiPolygon -> IO ()
  render r vp = do
    let xPoints = V.fromList . fmap ((*) 10 . round . fst) $ voronoiPolygonPoints vp
        yPoints = V.fromList . fmap ((*) 10 . round . snd) $ voronoiPolygonPoints vp
        xCentre = round $ (fst $ voronoiPolygonCentre vp) * 10
        yCentre = round $ (fst $ voronoiPolygonCentre vp) * 10
    SP.fillCircle r (V2 xCentre yCentre) 2 red
    SP.polygon r xPoints yPoints red



