{-# LANGUAGE InstanceSigs #-}
module Render  
  ( Render(..)
  , ColouredVPoly(..)
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
import Voronoi.JCVoronoi (JCVPoly(..), JCVEdge (jcvEdgePoint1))
import SDL.Primitive (Pos)
import Football.Understanding.Space.Data (SpacePoly(..))

white :: SP.Color
white = V4 255 255 255 255

red :: SP.Color
red = V4 255 0 0 255

redT :: SP.Color
redT = V4 255 0 0 20

orange :: SP.Color
orange = V4 255 165 0 255

blue :: SP.Color
blue = V4 0 0 255 255

blueT :: SP.Color
blueT = V4 0 0 255 20

purple :: SP.Color
purple = V4 255 0 255 255

grey :: SP.Color
grey = V4 155 155 155 255

class Render a where
  render :: S.Renderer -> a -> IO ()

instance Render Player where
  render :: S.Renderer -> Player -> IO ()
  render r p = do
    let ppv = playerPositionVector p
        scaled = (ppv ^. _xy ) * 20
        scaled' = fmap floor scaled
        colour = 
          case playerTeam p of
            Team1 -> red
            Team2 -> blue 
    renderIntention r scaled' (playerIntention p)
    SP.fillCircle r scaled' 10 colour

instance Render Ball where
  render :: S.Renderer -> Ball -> IO ()
  render r b = do
    let bpv = ballPositionVector b
        scaled = (bpv ^. _xy ) * 20
        scaled' = fmap floor scaled
    SP.fillCircle r scaled' 4 grey

instance Render VoronoiPolygon where
  render :: S.Renderer -> VoronoiPolygon -> IO ()
  render r vp = do
    let xPoints = V.fromList . fmap ((*) 20 . round . fst) $ voronoiPolygonPoints vp
        yPoints = V.fromList . fmap ((*) 20 . round . snd) $ voronoiPolygonPoints vp
        xCentre = round $ (fst $ voronoiPolygonCentre vp) * 20
        yCentre = round $ (fst $ voronoiPolygonCentre vp) * 20
    SP.fillCircle r (V2 xCentre yCentre) 4 red
    SP.polygon r xPoints yPoints red


data ColouredVPoly = ColouredVPoly
  { cvpColour :: SP.Color
  , cvpPoly :: JCVPoly
  }

instance Render ColouredVPoly where
  render :: S.Renderer -> ColouredVPoly -> IO ()
  render r (ColouredVPoly colour vp) = do
    let xPoints = V.fromList . fmap ((*) 20 . round . fst) $ fmap jcvEdgePoint1 $ polyEdges vp
        yPoints = V.fromList . fmap ((*) 20 . round . snd) $ fmap jcvEdgePoint1 $ polyEdges vp
        xCentre = round $ (fst $ polyPoint vp) * 20
        yCentre = round $ (snd $ polyPoint vp) * 20
    SP.fillCircle r (V2 xCentre yCentre) 2 colour
    SP.polygon r xPoints yPoints colour

instance Render SpacePoly where
  render :: S.Renderer -> SpacePoly -> IO ()
  render r (SpacePoly vp player) = do
    let xPoints = V.fromList . fmap ((*) 20 . round . fst) $ fmap jcvEdgePoint1 $ polyEdges vp
        yPoints = V.fromList . fmap ((*) 20 . round . snd) $ fmap jcvEdgePoint1 $ polyEdges vp
        xCentre = round $ (fst $ polyPoint vp) * 20
        yCentre = round $ (snd $ polyPoint vp) * 20
        colourT = 
          case playerTeam player of
            Team1 -> redT
            Team2 -> blueT 
        colour = 
          case playerTeam player of
            Team1 -> red
            Team2 -> blue
      
    SP.fillCircle r (V2 xCentre yCentre) 2 colour
    SP.fillPolygon r xPoints yPoints colourT

renderIntention :: S.Renderer -> Pos -> PlayerIntention -> IO()
renderIntention r pos (KickIntention (x, y)) = do
  SP.line r (V2 (floor $ x*20) (floor $ y*20)) pos purple
renderIntention r pos (MoveIntoSpace (x, y)) = do
  SP.line r (V2 (floor $ x*20) (floor $ y*20)) pos orange
renderIntention r pos ControlBallIntention = pure ()
renderIntention r pos (IntentionCooldown _) = pure ()
renderIntention r pos DoNothing            = pure ()




