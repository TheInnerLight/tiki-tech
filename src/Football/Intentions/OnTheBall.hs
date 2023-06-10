{-# LANGUAGE ScopedTypeVariables #-}

module Football.Intentions.OnTheBall where


import Football.Behaviours.Pass
import Football.Behaviours.Dribble
import Football.Match
import Football.Player
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (nearestSpace)
import Data.Foldable (find, traverse_, Foldable (foldl'), foldlM)
import Core (Log (logOutput), Random (randomRange, randomNormalMeanStd))
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import qualified Data.Ord
import Data.List (sortOn)
import Football.Behaviours.Shoot
import Football.Types
import qualified Data.Vector as Vector
import qualified Statistics.Distribution.Normal as ND
import Statistics.Distribution (Distribution(cumulative))
import Linear (V2(V2), project, normalize, Metric (dot), V3 (V3))

data OnTheBallCriteria =
  OnTheBallCriteria
    { otbcMinimumPassSafetyCoeff :: !(Maybe Double)
    , otbcMinimumPassDesirabilityCoeff :: !(Maybe Double)
    }

data OnTheBallOption 
  = DribbleOption DribbleDesirability
  | PassOption PassDesirability
  | ShotOption ShotDesirability
  deriving Show

onTheBallOptionDesirabilityCoeff :: OnTheBallOption -> Double
onTheBallOptionDesirabilityCoeff (DribbleOption dd) =
  let distXGAdded = (dribbleXGAdded dd - 0.000833333333333) / 0.009
      distOppXGAdded = (dribbleOppositionXGAdded dd + 0.000833333333333) / 0.05
      distSafety = (dribbleSafetyCoeff dd - 0.8) / 0.08
      v = V3 distXGAdded distSafety distOppXGAdded
      proj = (1/sqrt 3) * v `dot` V3 1 1 (-1)
      unitND = ND.normalDistr 0 1
  in cumulative unitND proj
onTheBallOptionDesirabilityCoeff (PassOption pd) =
  let distXGAdded = (passXGAdded pd - 0.000833333333333) / 0.01
      distOppXGAdded = (passOppositionXGAdded pd + 0.000833333333333) / 0.05
      distSafety = (passSafetyCoeff pd - 0.8) / 0.08
      v = V3 distXGAdded distSafety distOppXGAdded
      proj = (1/sqrt 3) * v `dot` V3 1 1 (-1)
      unitND = ND.normalDistr 0 1
  in cumulative unitND proj
onTheBallOptionDesirabilityCoeff (ShotOption sd) = 
  let xgND = ND.normalDistr 0.1 0.025
      xgPos = cumulative xgND (shotXG sd) 
  in xgPos

determineOnTheBallIntention :: (Monad m, Match m, Log m, Random m) => OnTheBallCriteria -> Player -> m PlayerIntention
determineOnTheBallIntention otbc player = do
  ball <- gameBall
  safePassingOptions <- safestPassingOptions player
  dribbleOptions <- desirableDribbleOptions player
  shotOptions <- filter (\so -> shotXG so >= 0.04) <$> desirableShotOptions player
  let combinedOptions = fmap ShotOption shotOptions ++ fmap PassOption safePassingOptions ++ fmap DribbleOption dribbleOptions
      validOptions = filter (not . isNaN . onTheBallOptionDesirabilityCoeff) combinedOptions
      sortedOptions = sortOn (Data.Ord.Down . onTheBallOptionDesirabilityCoeff) validOptions

  chosenOption <- pickFrom $ cycle sortedOptions

  pure $ case chosenOption of
    DribbleOption dd -> DribbleIntention (locate2D ball) (dribbleDirection dd)
    PassOption pd    -> PassIntention (passTarget pd) (locate2D ball) (passBallVector pd)
    ShotOption sd    -> ShootIntention (shotTarget sd) (locate2D ball) (shotBallVector sd)
  where
    minPassSafety = fromMaybe 0 $ otbcMinimumPassSafetyCoeff otbc
    minPassDesirability = fromMaybe (-1) $ otbcMinimumPassDesirabilityCoeff otbc


pickFrom :: (Monad m, Match m, Log m, Random m) => [OnTheBallOption] -> m OnTheBallOption
pickFrom xs = picker $ cycle xs
  where
    picker (x:opts) = do
      optionVal <- randomRange 0 1
      if optionVal <= onTheBallOptionDesirabilityCoeff x then
        pure x
      else
        pickFrom opts
    picker _ = error "Impossible"



