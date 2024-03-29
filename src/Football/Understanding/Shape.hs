{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Football.Understanding.Shape where

import Control.Lens ((^.))
import Football.Ball
import Football.Player
import Football.Match
import Linear (normalize, V3 (V3), Metric (dot, norm, distance), R1 (_x), R2 (_y), V2 (V2))
import Data.List (sortOn, minimumBy, reverse, foldl', find)
import qualified Data.Ord
import Voronoi.JCVoronoi (JCVPoly(..))
import Football.Locate2D (Locate2D(locate2D))
import qualified Data.Map as Map
import Core (Log(..), Cache)
import Football.Understanding.Space (centresOfPlay, offsideLine)
import Football.Pitch (pitchHalfwayLineX)
import Football.Types
import Football.Understanding.Space.Data (CentresOfPlayCache, CentresOfPlay (centresOfPlayBothTeams, centresOfPlayTeam1, centresOfPlayTeam2))
import Football.Understanding.Team (toTeamCoordinateSystem2D, fromTeamCoordinateSystem2D, inTeamCoordinateSystem, toTeamCoordinateSystem)
import Data.TypeLits (Nat, type (-))



data BWFormationLine (n :: Nat) where
  UnchangedFormationLine :: FormationLine n -> BWFormationLine n
  ReducedFormationLine :: FormationLine (n-1) -> BWFormationLine n

outOfPossessionFormationRelativeTo :: (Monad m, Match m, Log m) => Formation -> Double -> Double -> Player -> V2 Double -> m (V2 Double)
outOfPossessionFormationRelativeTo Formation { formationLine1 = fl1, formationLine2 = fl2, formationLine3 = fl3, formationLine4 = fl4, formationLine5 = fl5} verticalCompactness horizontalCompactness player centrePos = do
  (V2 centreX centreY) <- toTeamCoordinateSystem2D (playerTeamId player) centrePos
  pos <- 
    if isGoalKeeper player then
      pure $ inDirection  (-35) (-52.5) (-25*verticalCompactness) 0 (centreX, centreY)
    else do
      fl1' <- outOfPossessionFormationRelativeToLine fl1 horizontalCompactness player
      fl2' <- outOfPossessionFormationRelativeToLine fl2 horizontalCompactness player
      fl3' <- outOfPossessionFormationRelativeToLine fl3 horizontalCompactness player
      fl4' <- outOfPossessionFormationRelativeToLine fl4 horizontalCompactness player
      fl5' <- outOfPossessionFormationRelativeToLine fl5 horizontalCompactness player
      logOutput player
      logOutput (fl1', fl2', fl3', fl4', fl5')
      case (fl1', fl2', fl3', fl4', fl5')  of
        (Just hy, Nothing, Nothing, Nothing, Nothing) -> pure $ inDirection  15  (-52) (-15*verticalCompactness) hy (centreX, centreY)
        (Nothing, Just hy, Nothing, Nothing, Nothing) -> pure $ inDirection  25  (-52) (-10*verticalCompactness) hy (centreX, centreY)
        (Nothing, Nothing, Just hy, Nothing, Nothing) -> pure $ inDirection  35  (-52) (-5*verticalCompactness)  hy (centreX, centreY)
        (Nothing, Nothing, Nothing, Just hy, Nothing) -> pure $ inDirection  45  (-52) 0                         hy (centreX, centreY)
        (Nothing, Nothing, Nothing, Nothing, Just hy) -> pure $ inDirection  55  (-52) (10*verticalCompactness)  hy (centreX, centreY)
        (_, _, _, _, _) -> error "Impossible: Player appeared in the formation multiple times"
  fromTeamCoordinateSystem2D (playerTeamId player) pos
  where
    inDirection maxX minX diffX diffY (x, y) = V2 (max minX $ min maxX $ x+diffX) (y+diffY)

outOfPossessionFormationRelativeToLine :: (Monad m, Match m) => FormationLine n -> Double -> Player -> m (Maybe Double)
outOfPossessionFormationRelativeToLine line horizontalCompactness player = do
  (adjustedLine, excludedPlayer) <- excludeBallWinner line
  let chosenLine = 
        case excludedPlayer of
          Just ep | ep == player -> UnchangedFormationLine line
          _ -> adjustedLine
  case chosenLine of
    UnchangedFormationLine adjustedLine' ->
      case horizPos player adjustedLine' of
        Just p -> pure . Just $ (3 + 3 * horizontalCompactness) * p
        Nothing -> pure Nothing
    ReducedFormationLine adjustedLine' ->
      case horizPos player adjustedLine' of
        Just p -> pure . Just $ (3 + 3 * horizontalCompactness) * p
        Nothing -> pure Nothing

  where
    horizPos :: Player -> FormationLine n -> Maybe Double
    horizPos p'' (FiveLine p' _ _ _ _) | p'' == p' = Just $ -4
    horizPos p'' (FiveLine _ p' _ _ _) | p'' == p' = Just $ -2
    horizPos p'' (FiveLine _ _ p' _ _) | p'' == p' = Just $ 0
    horizPos p'' (FiveLine _ _ _ p' _) | p'' == p' = Just $ 2
    horizPos p'' (FiveLine _ _ _ _ p') | p'' == p' = Just $ 4
    horizPos p'' (FourLine p' _ _ _)   | p'' == p' = Just $ -3
    horizPos p'' (FourLine _ p' _ _)   | p'' == p' = Just $ -1
    horizPos p'' (FourLine _ _ p' _)   | p'' == p' = Just $ 1
    horizPos p'' (FourLine _ _ _ p')   | p'' == p' = Just $ 3
    horizPos p'' (ThreeLine p' _ _)    | p'' == p' = Just $ -2
    horizPos p'' (ThreeLine _ p' _)    | p'' == p' = Just $ -0
    horizPos p'' (ThreeLine _ _ p')    | p'' == p' = Just $ 2
    horizPos p'' (TwoLine p' _)        | p'' == p' = Just $ -1
    horizPos p'' (TwoLine _ p')        | p'' == p' = Just $ 1
    horizPos p'' (OneLine p')          | p'' == p' = Just $ 0
    horizPos _ _ = Nothing


excludeBallWinner :: (Monad m, Match m) => FormationLine n -> m (BWFormationLine n, Maybe Player)
excludeBallWinner (FiveLine p1 p2 p3 p4 p5) = do
  p1s <- getPlayerState p1
  p2s <- getPlayerState p2
  p3s <- getPlayerState p3
  p4s <- getPlayerState p4
  p5s <- getPlayerState p5
  case (playerStateIntention p1s, playerStateIntention p2s, playerStateIntention p3s, playerStateIntention p4s, playerStateIntention p5s) of
    (WinBallIntention, _, _, _, _) -> pure (ReducedFormationLine   $ FourLine p2 p3 p4 p5, Just p1)
    (_, WinBallIntention, _, _, _) -> pure (ReducedFormationLine   $ FourLine p1 p3 p4 p5, Just p2)
    (_, _, WinBallIntention, _, _) -> pure (ReducedFormationLine   $ FourLine p1 p2 p4 p5, Just p3)
    (_, _, _, WinBallIntention, _) -> pure (ReducedFormationLine   $ FourLine p1 p2 p4 p5, Just p4)
    (_, _, _, _, WinBallIntention) -> pure (ReducedFormationLine   $ FourLine p1 p2 p3 p4, Just p5)
    (_, _, _, _, _)                        -> pure (UnchangedFormationLine $ FiveLine p1 p2 p3 p4 p5, Nothing)
excludeBallWinner (FourLine p1 p2 p3 p4) = do
  p1s <- getPlayerState p1
  p2s <- getPlayerState p2
  p3s <- getPlayerState p3
  p4s <- getPlayerState p4
  case (playerStateIntention p1s, playerStateIntention p2s, playerStateIntention p3s, playerStateIntention p4s) of
    (WinBallIntention, _, _, _) -> pure (ReducedFormationLine $ ThreeLine p2 p3 p4, Just p1)
    (_, WinBallIntention, _, _) -> pure (ReducedFormationLine $ ThreeLine p1 p3 p4, Just p2)
    (_, _, WinBallIntention, _) -> pure (ReducedFormationLine $ ThreeLine p1 p2 p4, Just p3)
    (_, _, _, WinBallIntention) -> pure (ReducedFormationLine $ ThreeLine p1 p2 p3, Just p4)
    (_, _, _, _)                        -> pure (UnchangedFormationLine $ FourLine p1 p2 p3 p4, Nothing)
excludeBallWinner (ThreeLine p1 p2 p3) = do
  p1s <- getPlayerState p1
  p2s <- getPlayerState p2
  p3s <- getPlayerState p3
  case (playerStateIntention p1s, playerStateIntention p2s, playerStateIntention p3s) of
    (WinBallIntention, _, _) -> pure (ReducedFormationLine $ TwoLine p2 p3, Just p1)
    (_, WinBallIntention, _) -> pure (ReducedFormationLine $ TwoLine p1 p3, Just p2)
    (_, _, WinBallIntention) -> pure (ReducedFormationLine $ TwoLine p1 p2, Just p3)
    (_, _, _)                        -> pure (UnchangedFormationLine $ ThreeLine p1 p2 p3, Nothing)
excludeBallWinner (TwoLine p1 p2) = do
  p1s <- getPlayerState p1
  p2s <- getPlayerState p2
  case (playerStateIntention p1s, playerStateIntention p2s) of
    (WinBallIntention, _) -> pure (ReducedFormationLine $ OneLine p2, Just p1)
    (_, WinBallIntention) -> pure (ReducedFormationLine $ OneLine p1, Just p2)
    (_, _)                        -> pure (UnchangedFormationLine $ TwoLine p1 p2, Nothing)
excludeBallWinner (OneLine p1) = do
  p1s <- getPlayerState p1
  case playerStateIntention p1s of
    (WinBallIntention) -> pure (ReducedFormationLine EmptyLine, Just p1)
    _                          -> pure (UnchangedFormationLine $ OneLine p1, Nothing)
excludeBallWinner EmptyLine = pure (UnchangedFormationLine EmptyLine, Nothing)

outOfPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
outOfPossessionDesiredPosition player = do
  (V2 pCentreX pCentreY) <- case oppositionTeam (playerTeamId player) of
    TeamId1 -> centresOfPlayTeam1 <$> centresOfPlay
    TeamId2 -> centresOfPlayTeam2 <$> centresOfPlay
  (V2 ballX ballY) <- locate2D <$> gameBall
  pitch' <- pitch
  team <- getTeam $ playerTeamId player

  let (tCentreX, tCentreY) = (0.55*pCentreX+0.45*ballX, max (-15) $ min 15 $ 0.85*pCentreY+ 0.15*ballY)
      halfPitchLength = 0.5 * pitchLength pitch'
      horizontalCompactness = min halfPitchLength (halfPitchLength + tCentreX) / halfPitchLength
  pos <- outOfPossessionFormationRelativeTo (teamFormation team) 1 horizontalCompactness player (V2 tCentreX tCentreY)
  clampPitch pos

inPossessionDesiredPosition :: (Monad m, Match m, Log m, Cache m CentresOfPlayCache) => Player -> m (V2 Double)
inPossessionDesiredPosition player = do
  (V2 pCentreX pCentreY) <- centresOfPlayBothTeams <$> centresOfPlay
  ballXY <- locate2D <$> gameBall
  pitch' <- pitch
  offsideLineX <- offsideLine (playerTeamId player)
  attackingDirection' <- attackingDirection (playerTeamId player)

  (V2 centreX centreY) <- toTeamCoordinateSystem2D (playerTeamId player) $ V2 (0.6*pCentreX+0.4*(ballXY ^. _x)) (max (-15) $ min 15 $ 0.85*pCentreY + 0.15*(ballXY ^. _y))

  let offsideLineX' =
        case attackingDirection' of
          AttackingLeftToRight -> offsideLineX
          AttackingRightToLeft -> -offsideLineX
      halfPitchLength = 0.5 * pitchLength pitch'
      horizontalCompactness = min halfPitchLength (halfPitchLength + centreX) / halfPitchLength
    
  let pos = case playerNumber player of
            1 -> inDirection  (-20)                             (-25) 0                              (centreX, centreY)
            2 -> inDirection  offsideLineX'                     (5) 30                               (centreX, centreY)
            3 -> inDirection  offsideLineX'                     (5) (-30)                            (centreX, centreY)
            4 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) 10                             (centreX, centreY)
            5 -> inDirection  (pitchHalfwayLineX pitch' + 10)   (-15) (-10)                          (centreX, centreY)
            6 -> inDirection  (pitchHalfwayLineX pitch' + 17.5) (-5)  0                              (centreX, centreY)
            10 -> inDirection (pitchHalfwayLineX pitch' + 25)   (0)   (-15)                          (centreX, centreY)
            8 -> inDirection  (pitchHalfwayLineX pitch' + 25)   (0)   15                             (centreX, centreY)
            11 -> inDirection offsideLineX'                     (15)  (-8-12*horizontalCompactness)  (centreX, centreY)
            7 -> inDirection  offsideLineX'                     (15)  (8+12*horizontalCompactness)   (centreX, centreY)
            9 -> inDirection  offsideLineX'                     (15)  0                              (centreX, centreY)
            _ -> inDirection  offsideLineX'                     0     0                              (centreX, centreY)

  pos' <- fromTeamCoordinateSystem2D (playerTeamId player) pos
  clampPitch pos'
  where
    inDirection maxX diffX diffY (x, y) = V2 (min maxX $ x+diffX) (y+diffY)
