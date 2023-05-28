module Football.Intentions.OnTheBall where


import Football.Behaviours.Pass
import Football.Match
import Football.Player
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (nearestSpace)
import Data.Foldable (find, traverse_)
import Core (Log (logOutput))
import Data.Maybe (fromMaybe)

data OnTheBallCriteria =
  OnTheBallCriteria
    { otbcMinimumPassSafetyCoeff :: Maybe Double
    , otbcMinimumPassDesirabilityCoeff :: Maybe Double
    }

determineOnTheBallIntention :: (Monad m, Match m, Log m) => OnTheBallCriteria -> Player -> m PlayerIntention
determineOnTheBallIntention otbc player = do
  ball <- gameBall
  safePassingOptions <- safestPassingOptions player
  case find (\p -> passSafetyCoeff p >= minPassSafety && passDesirabilityCoeff p >= minPassDesirability) safePassingOptions of
    Just pass -> 
      case passTarget pass of
        PlayerTarget _ ->
          pure $ KickIntention (locate2D ball) (passBallVector pass)
        AheadOfTarget _ ->
          pure $ KickIntention (locate2D ball) (passBallVector pass)
        SpaceTarget _ ->
          pure $ KickIntention (locate2D ball) (passBallVector pass)
    Nothing -> do
      ns <- nearestSpace player
      pure $ DribbleIntention (locate2D ball) ns
  where
    minPassSafety = fromMaybe 0 $ otbcMinimumPassSafetyCoeff otbc
    minPassDesirability = fromMaybe (-1) $ otbcMinimumPassDesirabilityCoeff otbc