{-# LANGUAGE LambdaCase #-}

module Football.MatchStats where

import Football.Match
import Football.Types 
import Football.Events (passes, PassageOfPlay (passageOfPlayTeam, passageOfPlayTouches, passageOfPlayElements), passagesOfPlay, PlayElement (PassElement))
import Data.Foldable (foldr)
import Football.Maths (average)


passesCompleted :: (Match m, Monad m) => TeamId -> m (Int, Int)
passesCompleted teamId = do
  passes' <- passes
  let matchesTeam (CompletePass t _) = playerTeamId (touchOfBallPlayer t) == teamId
      matchesTeam (IncompletePass t) = playerTeamId (touchOfBallPlayer t) == teamId
  let teamPasses = filter matchesTeam passes'
  let folder (CompletePass _ _) (c, t) = (c+1, t+1)
      folder (IncompletePass _) (c, t) = (c, t+1)
  pure $ foldr folder (0, 0) teamPasses
  

oppositionPassesPerDefensiveAction :: (Match m, Monad m) => TeamId -> m Double
oppositionPassesPerDefensiveAction teamId = do
  pops <- passagesOfPlay
  let oppositionPassages = filter (\pop -> passageOfPlayTeam pop /= teamId) pops
  pure $ average $ fmap (fromIntegral . length . filter (\case PassElement _ -> True; _ -> False) . passageOfPlayElements) oppositionPassages


