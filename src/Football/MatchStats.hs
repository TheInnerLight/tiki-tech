module Football.MatchStats where

import Football.Match
import Football.Types 
import Football.Events (passes)
import Data.Foldable (foldr)


passesCompleted :: (Match m, Monad m) => TeamId -> m (Int, Int)
passesCompleted teamId = do
  passes' <- passes
  let matchesTeam (CompletePass t _) = playerTeamId (touchOfBallPlayer t) == teamId
      matchesTeam (IncompletePass t) = playerTeamId (touchOfBallPlayer t) == teamId
  let teamPasses = filter matchesTeam passes'
  let folder (CompletePass _ _) (c, t) = (c+1, t+1)
      folder (IncompletePass _) (c, t) = (c, t+1)
  pure $ foldr folder (0, 0) teamPasses
  




