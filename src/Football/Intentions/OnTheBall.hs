module Football.Intentions.OnTheBall where


import Football.Behaviours.Pass
import Football.Behaviours.Dribble
import Football.Match
import Football.Player
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (nearestSpace)
import Data.Foldable (find, traverse_)
import Core (Log (logOutput))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Ord
import Data.List (sortOn)
import Football.Behaviours.Shoot
import Football.Types

data OnTheBallCriteria =
  OnTheBallCriteria
    { otbcMinimumPassSafetyCoeff :: !(Maybe Double)
    , otbcMinimumPassDesirabilityCoeff :: !(Maybe Double)
    }

data OnTheBallOption 
  = DribbleOption DribbleDesirability
  | PassOption PassDesirability
  | ShotOption ShotDesirability

onTheBallActionSafetyCoeff :: OnTheBallOption -> Double
onTheBallActionSafetyCoeff (DribbleOption dd) = dribbleSafetyCoeff dd
onTheBallActionSafetyCoeff (PassOption pd) = passSafetyCoeff pd
onTheBallActionSafetyCoeff (ShotOption sd) = shotSafetyCoeff sd

onTheBallOptionDesirabilityCoeff :: OnTheBallOption -> Double
onTheBallOptionDesirabilityCoeff (DribbleOption dd) = dribbleDesirabilityCoeff dd
onTheBallOptionDesirabilityCoeff (PassOption pd) = passDesirabilityCoeff pd
onTheBallOptionDesirabilityCoeff (ShotOption sd) = shotDesirabilityCoeff sd

determineOnTheBallIntention :: (Monad m, Match m, Log m) => OnTheBallCriteria -> Player -> m PlayerIntention
determineOnTheBallIntention otbc player = do
  ball <- gameBall
  safePassingOptions <- safestPassingOptions player
  dribbleOptions <- desirableDribbleOptions player
  shotOptions <- filter (\so -> shotXG so >= 0.04) <$> desirableShotOptions player
  let combinedOptions = fmap PassOption safePassingOptions ++ fmap DribbleOption dribbleOptions ++ fmap ShotOption shotOptions
      sortedOptions = sortOn (Data.Ord.Down . onTheBallOptionDesirabilityCoeff) combinedOptions
      eligibleOptions = filter (\p -> onTheBallActionSafetyCoeff p >= minPassSafety && onTheBallOptionDesirabilityCoeff p >= minPassDesirability) sortedOptions
      chosenOption = fromMaybe (head sortedOptions) $ listToMaybe eligibleOptions
  pure $ case chosenOption of
    DribbleOption dd -> DribbleIntention (locate2D ball) (dribbleDirection dd)
    PassOption pd    -> PassIntention (passTarget pd) (locate2D ball) (passBallVector pd)
    ShotOption sd   -> ShootIntention (shotTarget sd) (locate2D ball) (shotBallVector sd)
  where
    minPassSafety = fromMaybe 0 $ otbcMinimumPassSafetyCoeff otbc
    minPassDesirability = fromMaybe (-1) $ otbcMinimumPassDesirabilityCoeff otbc
