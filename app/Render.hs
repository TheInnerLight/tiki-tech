{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Render  
  ( Render(..)
  , Pitch(..)
  , OffsideLine(..)
  , DefLine(..)
  , StatsBoard(..)
  ) where

import qualified SDL as S
import qualified SDL.Primitive as SP
import qualified SDL.Font as SDLFont
import SDL.Vect             (V2(..), V3(..), V4(..))
import Football.Player
import Football.Ball
import Control.Lens ((^.))
import Linear.V3 (_xy, _x, _y)
import qualified Data.Vector.Storable as V
import Voronoi.JCVoronoi (JCVPoly(..), JCVEdge (jcvEdgePoint1), voronoiPolygonArea)
import SDL.Primitive (Pos)
import Football.Understanding.Space.Data (SpacePoly(..))
import Foreign.C (CInt)
import qualified Data.Vector as VE
import Football.Types
import qualified Data.Text as T
import qualified SDL.Video.Renderer as SVR
import Football.Locate2D (Locate2D(locate2D))
import Data.Text as T
import qualified Text.Printf as TText

white :: SP.Color
white = V4 255 255 255 255

red :: SP.Color
red = V4 200 15 47 255

redT :: SP.Color
redT = V4 200 15 47 20

darkRed :: SP.Color
darkRed = V4 100 0 0 255

darkRedT :: SP.Color
darkRedT = V4 100 0 0 20

orange :: SP.Color
orange = V4 255 165 0 255

blue :: SP.Color
blue = V4 108 174 222 255

darkBlue :: SP.Color
darkBlue = V4 0 0 100 255

darkBlueT :: SP.Color
darkBlueT = V4 0 0 100 20

green :: SP.Color
green = V4 9 50 9 255

cyan :: SP.Color
cyan = V4 0 255 255 255

blueT :: SP.Color
blueT = V4 108 174 222 20

purple :: SP.Color
purple = V4 255 0 255 255

pink :: SP.Color
pink = V4 255 192 203 255

grey :: SP.Color
grey = V4 155 155 155 255

class Render a where
  render :: S.Renderer -> SDLFont.Font -> a -> IO ()

scaleFactor :: Double
scaleFactor = 20.0


data Scoreboard =
  Scoreboard
    { scoreboardTeamName1 :: T.Text
    , scoreboardTeamName2 :: T.Text
    }


data StatsBoard =
  StatsBoard
    { statsBoardTeamId :: TeamId
    , statsBoardPassesCompleted :: Int
    , statsBoardPassesAttempted :: Int
    , statsBoardOppPPDA :: Double
    }

instance Render StatsBoard where
  render :: S.Renderer -> SDLFont.Font -> StatsBoard -> IO ()
  render r font board = do
    let scaled' = 
          case statsBoardTeamId board of
            TeamId1 -> coordinateTransPV (-20,  -30)
            TeamId2 -> coordinateTransPV (20,  -30)
        completionPercentage :: Double = 100.0 * fromIntegral (statsBoardPassesCompleted board) / fromIntegral (statsBoardPassesAttempted board)
        colour = 
          case statsBoardTeamId board of
            TeamId1 -> red
            TeamId2 -> blue
        text = T.pack $
           "Passes Completed: " <> show (statsBoardPassesCompleted board) <> " / " <> show (statsBoardPassesAttempted board) <> "  (" <> TText.printf "%.2f" completionPercentage <> "%)\n"
           <> "PPDA: " <> TText.printf "%.2f" (statsBoardOppPPDA board)
    surf <- SDLFont.blendedWrapped font colour 500 text
    shirtNumberTexture <- SVR.createTextureFromSurface r surf
    surfDimensions <- SVR.surfaceDimensions surf
    SVR.freeSurface surf
    let textOffset = floor <$> (\x -> x/2) <$> fromIntegral <$> surfDimensions
    let targetRect = S.Rectangle (S.P $ scaled' - textOffset) surfDimensions
    SVR.copy r shirtNumberTexture Nothing (Just targetRect)
    S.destroyTexture shirtNumberTexture

coordinateTransV :: (Integral b, S.R2 t) => t Double -> V2 b
coordinateTransV v = 
  V2 (floor $ (v ^. _x +57.5)*scaleFactor) (floor $ (v ^. _y+36.5) * scaleFactor) 
  --fmap floor $ (v ^. _xy ) * pure scaleFactor + pure scaleFactor * V2 5.0 2.5


coordinateTransP :: (Integral a, Integral b) => (V2 Double) -> (a, b)
coordinateTransP (V2 x y) = 
  (floor $ (x+57.5)*scaleFactor, floor $ (y+36.5) * scaleFactor) 


coordinateTransPV :: Integral a => (Double, Double) -> V2 a
coordinateTransPV (x, y) = 
  V2 (floor $ (x+57.5)*scaleFactor) (floor $ (y+36.5) * scaleFactor) 

instance Render PlayerState where
  render :: S.Renderer -> SDLFont.Font -> PlayerState -> IO ()
  render r font p = do
    let ppv = playerStatePositionVector p
        scaled' = coordinateTransV ppv
        colour = 
          case playerTeamId $ playerStatePlayer p of
            TeamId1 -> red
            TeamId2 -> blue 

    renderIntention r scaled' (playerStateIntention p)
    SP.fillCircle r scaled' 10 colour

    surf <- SDLFont.solid font white (T.pack (show $ playerNumber $ playerStatePlayer p))
    shirtNumberTexture <- SVR.createTextureFromSurface r surf
    surfDimensions <- SVR.surfaceDimensions surf
    SVR.freeSurface surf
    let textOffset = floor <$> (\x -> x/2) <$> fromIntegral <$> surfDimensions
    let targetRect = S.Rectangle (S.P $ scaled' - textOffset) surfDimensions
    SVR.copy r shirtNumberTexture Nothing (Just targetRect)
    S.destroyTexture shirtNumberTexture

    

instance Render Ball where
  render :: S.Renderer -> SDLFont.Font -> Ball -> IO ()
  render r _ b = do
    let bpv = ballPositionVector b
        scaled' = coordinateTransV bpv
    SP.fillCircle r scaled' 4 grey

instance Render SpacePoly where
  render :: S.Renderer-> SDLFont.Font -> SpacePoly -> IO ()
  render r _ (SpacePoly vp player) = do
--    pure ()
    let 
        (xPoints, yPoints) = unzip . fmap (coordinateTransP . jcvEdgePoint1) $ polyEdges vp
        (xCentre, yCentre) = coordinateTransP  $ polyPoint vp
        colourT = 
          case playerTeamId player of
            TeamId1 | voronoiPolygonArea vp <= 25 -> darkRedT
            TeamId1 -> redT
            TeamId2 | voronoiPolygonArea vp <= 25 -> darkBlueT
            TeamId2 -> blueT
        colour = 
          case playerTeamId player of
            TeamId1 | voronoiPolygonArea vp <= 25 -> darkRed
            TeamId1 -> red
            TeamId2 | voronoiPolygonArea vp <= 25 -> darkBlue
            TeamId2 -> blue
      
    SP.fillCircle r (V2 xCentre yCentre) 2 colour
    SP.fillPolygon r (V.fromList xPoints) (V.fromList yPoints) colourT
    SP.polygon r (V.fromList xPoints) (V.fromList yPoints) colour

instance Render Pitch where
  render r _ (Pitch length width) = do
    let pMin = coordinateTransPV            (-length/2,         -width/2)
    let pMax = coordinateTransPV            (length/2,          width/2)
    let halfwayMin = coordinateTransPV      (0,                 -width/2)
    let halfwayMax = coordinateTransPV      (0,                 width/2)
    let centreSpot = coordinateTransPV      (0,                 0)
    let goal1Min = coordinateTransPV        (-length/2-2.4,     -7.32/2)
    let goal1Max = coordinateTransPV        (-length/2,         7.32/2)
    let goalArea1Min = coordinateTransPV    (-length/2,         -7.32/2-5.5)
    let goalArea1Max = coordinateTransPV    (-length/2+5.5,     7.32/2+5.5)
    let penaltyArea1Min = coordinateTransPV (-length/2,         -7.32/2-5.5-11)
    let penaltyArea1Max = coordinateTransPV (-length/2+5.5+11,  7.32/2+5.5+11)
    let penalltySpot1 = coordinateTransPV   (-length/2+11,      0)
    let goal2Min = coordinateTransPV        (length/2+2.4,      -7.32/2)
    let goal2Max = coordinateTransPV        (length/2,          7.32/2)
    let goalArea2Min = coordinateTransPV    (length/2-5.5,      -7.32/2-5.5)
    let goalArea2Max = coordinateTransPV    (length/2,          7.32/2+5.5)
    let penaltyArea2Min = coordinateTransPV (length/2-5.5-11,   -7.32/2-5.5-11)
    let penaltyArea2Max = coordinateTransPV (length/2,          7.32/2+5.5+11)
    let penalltySpot2 = coordinateTransPV   (length/2-11,       0)
    let cornerTL = coordinateTransPV        (-length/2,         -width/2)
    let cornerTR = coordinateTransPV        (length/2,          -width/2)
    let cornerBL = coordinateTransPV        (-length/2,         width/2)
    let cornerBR = coordinateTransPV        (length/2,          width/2)
    let outerPMin = coordinateTransPV       (-length/2-5,       -width/2-2.5)
    let outerPMax = coordinateTransPV       (length/2+5,        width/2+2.5)
    SP.fillRectangle r outerPMin outerPMax green
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
renderIntention r pos i = pure ()
-- renderIntention r pos (PassIntention t ip p _) = do
--   let iceptLoc = coordinateTransV ip
--   SP.line r iceptLoc pos cyan
--   case t of
--     PlayerTarget tp -> SP.line r pos (coordinateTransV $ locate2D tp) pink
--     SpaceTarget st -> SP.line r pos (coordinateTransV $ locate2D st) darkRed
--     AheadOfTarget at -> SP.line r pos (coordinateTransV $ locate2D at) orange
--     _               -> pure ()
-- renderIntention r pos (MoveIntoSpace p _) = do
--   let spaceLoc = coordinateTransV p
--   SP.line r spaceLoc pos orange
-- renderIntention :: S.Renderer -> Pos -> PlayerIntention -> IO()
-- renderIntention r pos (PassIntention _ ip p) = do
--   let iceptLoc = coordinateTransPV ip
--   SP.line r iceptLoc pos pink
-- renderIntention r pos (ShootIntention _ ip p) = do
--   let iceptLoc = coordinateTransPV ip
--   SP.line r iceptLoc pos purple
-- renderIntention r pos (DribbleIntention ip p) = do
--   let iceptLoc = coordinateTransPV ip
--   --let kickLoc = coordinateTransV (ip + p)
--   SP.line r iceptLoc pos cyan
--   --SP.line r kickLoc pos cyan
-- renderIntention r pos (MoveIntoSpace p _) = do
--   let spaceLoc = coordinateTransPV p
--   SP.line r spaceLoc pos orange
-- renderIntention r pos (ControlBallIntention p _) = do
--   let spaceLoc = coordinateTransPV p
--   SP.line r spaceLoc pos pink
-- renderIntention r pos (IntentionCooldown _) = pure ()
-- renderIntention r pos (RunToLocation loc _) = do
--   let spaceLoc = coordinateTransPV loc
--   SP.line r spaceLoc pos cyan
-- renderIntention r pos DoNothing             = pure ()
-- renderIntention r pos _                     = pure ()



newtype OffsideLine = OffsideLine Double

instance Render OffsideLine where
  render :: S.Renderer -> SDLFont.Font -> OffsideLine -> IO ()
  render r _ (OffsideLine x) = do
    let lp1 = coordinateTransV $ V2 x (-34)
    let lp2 = coordinateTransV $ V2 x 34
    SP.line r lp1 lp2 grey


data DefLine = DefLine (V2 Double) (V2 Double)

instance Render DefLine where
  render :: S.Renderer -> SDLFont.Font -> DefLine -> IO ()
  render r _ (DefLine p1 p2) = do
    let lp1 = coordinateTransV p1
    let lp2 = coordinateTransV p2
    SP.line r lp1 lp2 grey
