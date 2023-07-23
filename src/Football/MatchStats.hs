{-# LANGUAGE LambdaCase #-}

module Football.MatchStats where

import Football.Match
import Football.Types 
import Football.Events (passes, PassageOfPlay (passageOfPlayTeam, passageOfPlayTouches, passageOfPlayElements), passagesOfPlay, PlayElement (PassElement), touchEvents)
import Data.Foldable (foldr)
import Football.Maths (average)
import Data.List (partition)
import Data.Maybe (listToMaybe)
import Football.GameTime (gameTimeSeconds)
import Football.Understanding.Team (advancementCoeff)
import Control.Monad (filterM)

shots :: (Match m, Monad m) => TeamId -> m Int
shots teamId = 
  length . filter isTeamShotTouch <$> touchEvents
  where
    isTeamShotTouch t = 
      case (touchOfBallType t, playerTeamId $ touchOfBallPlayer t) of
        (ShotTouch, teamId') | teamId' == teamId -> True
        _                                        -> False

passesCompleted :: (Match m, Monad m) => TeamId -> m (Int, Int)
passesCompleted teamId = do
  passes' <- passes
  let matchesTeam (CompletePass t _) = playerTeamId (touchOfBallPlayer t) == teamId
      matchesTeam (IncompletePass t) = playerTeamId (touchOfBallPlayer t) == teamId
  let teamPasses = filter matchesTeam passes'
  let folder (CompletePass _ _) (c, t) = (c+1, t+1)
      folder (IncompletePass _) (c, t) = (c, t+1)
  pure $ foldr folder (0, 0) teamPasses

pitchTilt :: (Match m, Monad m) => TeamId -> m Double
pitchTilt teamId = do
  passes' <- passes
  let matchesTeam (CompletePass t _) = True
      matchesTeam (IncompletePass _) = False
  let (teamPasses, oppositionPasses) = partition (\(CompletePass t _) ->  playerTeamId (touchOfBallPlayer t) == teamId) $  filter matchesTeam passes'
  finalThirdTeamPasses <- length <$> filterM isFinalThirdPass teamPasses
  finalThirdOppositionPasses <- length <$>  filterM isFinalThirdPass oppositionPasses
  if finalThirdTeamPasses + finalThirdOppositionPasses == 0 then
    pure 0.5
  else
    pure $ fromIntegral finalThirdTeamPasses / fromIntegral (finalThirdTeamPasses + finalThirdOppositionPasses)
  where 
    isFinalThirdPass (CompletePass t _) = (> 0.667) <$> advancementCoeff (playerTeamId $ touchOfBallPlayer t) (touchOfBallLocation t)
    isFinalThirdPass _                  = pure False

possession :: (Match m, Monad m) => TeamId -> m Double
possession teamId = do
  pops <- passagesOfPlay
  let (oppositionPassages, teamPassages) = partition (\pop -> passageOfPlayTeam pop /= teamId) pops
      teamTimeInPosession = sum $ fmap (timeInPossession . passageOfPlayTouches) teamPassages
      oppositionTimeInPossession = sum $ fmap (timeInPossession . passageOfPlayTouches) oppositionPassages
  pure $ teamTimeInPosession / (oppositionTimeInPossession + teamTimeInPosession)
  where 
    timeInPossession [] = 0
    timeInPossession [_] = 0
    timeInPossession xs = gameTimeSeconds (touchOfBallTime (last xs)) - gameTimeSeconds (touchOfBallTime (head xs))

oppositionPassesPerDefensiveAction :: (Match m, Monad m) => TeamId -> m Double
oppositionPassesPerDefensiveAction teamId = do
  pops <- passagesOfPlay
  let (oppositionPassages, teamPassages) = partition (\pop -> passageOfPlayTeam pop /= teamId) pops
  let turnovers = length $ filter (isTurnover . listToMaybe . passageOfPlayTouches) teamPassages
  pure $ sum (fmap (fromIntegral . length . filter (\case PassElement _ -> True; _ -> False) . passageOfPlayElements) oppositionPassages) / fromIntegral turnovers
  where 
    isTurnover (Just t) = 
      case touchOfBallType t of
        InterceptionTouch -> True
        TackleTouch -> True
        _ -> False
    isTurnover Nothing = False
      


