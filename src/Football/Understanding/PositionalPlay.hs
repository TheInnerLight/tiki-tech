{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Football.Understanding.PositionalPlay where

import Control.Lens ((^.))
import Football.Types
import Football.Match
import Core (Log, Cache)
import Football.Understanding.Space.Data
import Linear (V2(..), R1 (_x))
import Football.Understanding.Space (centresOfPlay, offsideLine, pitchHorizontalZone)
import Football.Locate2D (Locate2D(locate2D))
import Football.Understanding.Team (toTeamCoordinateSystem2D)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Data.Foldable (foldl')
import Data.Maybe (isJust, isNothing)

data VerticalZone
  = DefZone
  | DefMidZone
  | MidZone
  | AttMidZone
  | AttZone
  deriving (Eq, Ord)

allVerticalZones :: [VerticalZone]
allVerticalZones = [DefZone, DefMidZone, MidZone, AttMidZone, AttZone]

allHorizontalZones :: [HorizontalZone]
allHorizontalZones = [WingHZ LeftHalf, HalfSpaceHZ LeftHalf, CentreHZ, HalfSpaceHZ RightHalf, WingHZ RightHalf]

allZones :: [(VerticalZone, HorizontalZone)]
allZones = liftM2 (,) allVerticalZones allHorizontalZones

zoneOccupation :: Map (VerticalZone, HorizontalZone) [PlayerState] -> (VerticalZone, HorizontalZone) -> (Int, Int)
zoneOccupation map' (vz, hz) =
  (foldl' f 0 allVerticalZones, foldl' f2 0 allHorizontalZones)
  where 
    f c vz' = 
      case Map.lookup (vz', hz) map' of
        Just _ -> c + 1
        Nothing -> c
    f2 c hz' = 
      case Map.lookup (vz, hz') map' of
        Just _ -> c + 1
        Nothing -> c

verticalZone :: (Monad m, Match m, Locate2D x) => TeamId -> x -> m VerticalZone
verticalZone teamId x = do
  offsideLineX <- offsideLine teamId

  offsideXY <- toTeamCoordinateSystem2D teamId $ V2 offsideLineX 0
  posXY <- toTeamCoordinateSystem2D teamId $ locate2D x

  if posXY ^. _x > offsideXY ^. _x - 6 then
    pure AttZone
  else if posXY ^. _x > offsideXY ^. _x - 12 then
    pure AttMidZone
  else if posXY ^. _x > offsideXY ^. _x - 18 then
    pure MidZone
  else if posXY ^. _x > offsideXY ^. _x - 24 then
    pure DefMidZone
  else
    pure DefZone

positionalPlayZone :: (Monad m, Match m, Locate2D x) => TeamId -> x -> m (VerticalZone, HorizontalZone)
positionalPlayZone teamId x = (,) <$> verticalZone teamId x <*> pitchHorizontalZone teamId x


inPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
inPossessionDesiredPosition player = do
  (V2 pCentreX pCentreY) <- centresOfPlayBothTeams <$> centresOfPlay
  ballXY <- locate2D <$> gameBall
  pitch' <- pitch
  offsideLineX <- offsideLine (playerTeamId player)
  attackingDirection' <- attackingDirection (playerTeamId player)

  playerSt <- getPlayerState player

  allTeamPlayers <- teamPlayers (playerTeamId player)

  zonesAndPlayers <- traverse (\p -> (, p) <$> positionalPlayZone (playerTeamId player) p )  allTeamPlayers
  let zonePlayers = Map.map (fmap snd) $ groupByKey fst zonesAndPlayers


  let validZonesToMoveTo = filter(\z -> isValid (zoneOccupation zonePlayers z) && isNothing (Map.lookup z zonePlayers)) allZones


  pure $ V2 0 0
  where 
    isValid (vzCount, hzCount) = vzCount <= 3 && hzCount <= 2
  

groupByKey :: (Ord k) => (v -> k) -> [v] -> Map k [v]
groupByKey getkey
  = Map.fromListWith (++) . fmap (\val -> (getkey val, [val]))
