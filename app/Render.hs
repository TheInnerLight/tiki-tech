{-# LANGUAGE InstanceSigs #-}
module Render  
  ( Render(..)
  , Pitch(..)
  ) where

import qualified SDL as S
import qualified SDL.Primitive as SP
import SDL.Vect             (V2(..), V3(..), V4(..))
import Football.Player
import Football.Ball
import Control.Lens ((^.))
import Linear.V3 (_xy)
import qualified Data.Vector.Storable as V
import Voronoi.JCVoronoi (JCVPoly(..), JCVEdge (jcvEdgePoint1))
import SDL.Primitive (Pos)
import Football.Understanding.Space.Data (SpacePoly(..))
import Foreign.C (CInt)
import qualified Data.Vector as VE

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

pink :: SP.Color
pink = V4 255 192 203 255

grey :: SP.Color
grey = V4 155 155 155 255

class Render a where
  render :: S.Renderer -> a -> IO ()

scaleFactor :: Double
scaleFactor = 20.0


coordinateTransV :: (Integral b, S.R2 t) => t Double -> V2 b
coordinateTransV v = 
  fmap floor $ (v ^. _xy ) * pure scaleFactor + pure scaleFactor * V2 5.0 0


coordinateTransP :: (Integral a, Integral b) => (Double, Double) -> (a, b)
coordinateTransP (x, y) = 
  (floor $ (x+5)*scaleFactor, floor $ y * scaleFactor) 


coordinateTransPV :: Integral a => (Double, Double) -> V2 a
coordinateTransPV (x, y) = 
  V2 (floor $ (x+5)*scaleFactor) (floor $ y * scaleFactor) 

instance Render Player where
  render :: S.Renderer -> Player -> IO ()
  render r p = do
    let ppv = playerPositionVector p
        scaled' = coordinateTransV ppv
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
        scaled' = coordinateTransV bpv
    SP.fillCircle r scaled' 4 grey

instance Render SpacePoly where
  render :: S.Renderer -> SpacePoly -> IO ()
  render r (SpacePoly vp player) = do
    let 
        (xPoints, yPoints) = unzip . fmap (coordinateTransP . jcvEdgePoint1) $ polyEdges vp
        (xCentre, yCentre) = coordinateTransP  $ polyPoint vp
        colourT = 
          case playerTeam player of
            Team1 -> redT
            Team2 -> blueT 
        colour = 
          case playerTeam player of
            Team1 -> red
            Team2 -> blue
      
    SP.fillCircle r (V2 xCentre yCentre) 2 colour
    SP.fillPolygon r (V.fromList xPoints) (V.fromList yPoints) colourT
    SP.polygon r (V.fromList xPoints) (V.fromList yPoints) colour

data Pitch = Pitch

instance Render Pitch where
  render r Pitch = do
    let pMin = coordinateTransPV (0, 0)
    let pMax = coordinateTransPV (105, 68)
    let halfwayMin = coordinateTransPV (52.5, 0)
    let halfwayMax = coordinateTransPV (52.5, 68)
    let centreSpot = coordinateTransPV (52.5, 34)
    let goal1Min = coordinateTransPV (-2.4, 34-7.32/2)
    let goal1Max = coordinateTransPV (0, 34+7.32/2)

    let goalArea1Min = coordinateTransPV (0, 34-7.32/2-5.5)
    let goalArea1Max = coordinateTransPV (5.5, 34+7.32/2+5.5)
    let penaltyArea1Min = coordinateTransPV (0, 34-7.32/2-5.5-11)
    let penaltyArea1Max = coordinateTransPV (5.5+11, 34+7.32/2+5.5+11)
    let penalltySpot1 = coordinateTransPV (11, 34)
    let goal2Min = coordinateTransPV (107.4, 34-7.32/2)
    let goal2Max = coordinateTransPV (105, 34+7.32/2)
    let goalArea2Min = coordinateTransPV (105-5.5, 34-7.32/2-5.5)
    let goalArea2Max = coordinateTransPV (105, 34+7.32/2+5.5)
    let penaltyArea2Min = coordinateTransPV (105-5.5-11, 34-7.32/2-5.5-11)
    let penaltyArea2Max = coordinateTransPV (105, 34+7.32/2+5.5+11)
    let penalltySpot2 = coordinateTransPV (105-11, 34)
    let cornerTL = coordinateTransPV (0, 0)
    let cornerTR = coordinateTransPV (105, 0)
    let cornerBL = coordinateTransPV (0, 68)
    let cornerBR = coordinateTransPV (105, 68)
    SP.rectangle r pMin pMax white
    SP.line r halfwayMin halfwayMax white
    SP.fillCircle r centreSpot (floor $ scaleFactor * 0.22) white
    SP.circle r centreSpot (floor $ scaleFactor * 9.15) white
    SP.rectangle r goal1Min goal1Max white
    SP.rectangle r goalArea1Min goalArea1Max white
    SP.rectangle r goalArea2Min goalArea2Max white
    SP.rectangle r penaltyArea1Min penaltyArea1Max white
    SP.rectangle r penaltyArea2Min penaltyArea2Max white
    SP.rectangle r goal2Min goal2Max white
    SP.fillCircle r penalltySpot1 (floor $ scaleFactor * 0.22) white
    SP.fillCircle r penalltySpot2 (floor $ scaleFactor * 0.22) white
    SP.arc r penalltySpot1 (floor $ scaleFactor * 9.15) (-53) 53  white
    SP.arc r penalltySpot2 (floor $ scaleFactor * 9.15) 127 233  white
    SP.arc r cornerTL (floor $ scaleFactor * 1) 0 90  white
    SP.arc r cornerTR (floor $ scaleFactor * 1) 90 180  white
    SP.arc r cornerBL (floor $ scaleFactor * 1) (-90) 0  white
    SP.arc r cornerBR (floor $ scaleFactor * 1) (-180) (-90)  white

renderIntention :: S.Renderer -> Pos -> PlayerIntention -> IO()
renderIntention r pos (KickIntention ip p) = do
  let iceptLoc = coordinateTransPV ip
  let kickLoc = coordinateTransPV p
  SP.line r iceptLoc pos pink
  SP.line r kickLoc iceptLoc purple
renderIntention r pos (MoveIntoSpace p) = do
  let spaceLoc = coordinateTransPV p
  SP.line r spaceLoc pos orange
renderIntention r pos (ControlBallIntention p) = do
  let spaceLoc = coordinateTransPV p
  SP.line r spaceLoc pos pink
renderIntention r pos (IntentionCooldown _) = pure ()
renderIntention r pos DoNothing            = pure ()




