{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Intentions.OnTheBall where


import Football.Behaviours.Pass
import Football.Behaviours.Dribble
import Football.Match
import Football.Player
import Football.Locate2D (Locate2D(locate2D))
import Football.Behaviours.FindSpace (nearestSpace)
import Data.Foldable (find, traverse_, Foldable (foldl'), foldlM)
import Core (Log (logOutput, logFile), Random (randomRange, randomNormalMeanStd), Cache)
import Data.Maybe (fromMaybe, listToMaybe, fromJust)
import qualified Data.Ord
import Data.List (sortOn)
import Football.Behaviours.Shoot
import Football.Types
import qualified Data.Vector as Vector
import qualified Statistics.Distribution.Normal as ND
import Statistics.Distribution (Distribution(cumulative))
import Linear (V2(V2), project, normalize, Metric (dot), V3 (V3), V4 (V4))
import Data.Time.Clock.System (SystemTime(systemNanoseconds))
import Football.GameTime (gameTimeAddSeconds)
import Football.Understanding.Space.Data (SpaceCache)

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

cumulativeProbabilityValue :: Double -> Double -> Double -> Double 
cumulativeProbabilityValue mean sd value =
  let standardisedDiff = (value - mean) / sd
      nd = ND.normalDistr mean sd
  in cumulative nd standardisedDiff

onTheBallOptionDesirabilityCoeff :: OnTheBallOption -> Double
onTheBallOptionDesirabilityCoeff (DribbleOption dd) =
  let zXGAdded = min 3.5 $ (dribbleXGAdded dd * dribbleSafetyCoeff dd - 0.000833333333333) / 0.09
      zOppXGAdded = min 3.5 $ (dribbleOppositionXGAdded dd * (1 - dribbleSafetyCoeff dd) + 0.000833333333333) / 0.15
      zSafety = min 3.5 $ (dribbleSafetyCoeff dd - 1) / 0.01
      v = V3 zXGAdded zSafety zOppXGAdded
      proj = (1/sqrt 3) * v `dot` V3 1 1 (-1)
      unitND = ND.normalDistr 0 1
  in cumulative unitND proj
onTheBallOptionDesirabilityCoeff (PassOption pd) =
  let zXGAdded = min 3.5 $ (passXGAdded pd * passSafetyCoeff pd - 0.000833333333333) / 0.1
      zOppXGAdded = min 3.5 $ (passOppositionXGAdded pd * (1 - passSafetyCoeff pd)  +  0.000833333333333) / 0.15
      zSafety = min 3.5 $ (passSafetyCoeff pd - 0.85) / 0.08
      zLinesBroken = min 3.5 $ (passBrokenLines pd - 0.2) / 0.25
      v = V4 zXGAdded zSafety zOppXGAdded zLinesBroken
      proj = (1/sqrt 4) * v `dot` V4 1 1 (-1) 1
      unitND = ND.normalDistr 0 1
  in cumulative unitND proj
onTheBallOptionDesirabilityCoeff (ShotOption sd) = 
  let xgND = ND.normalDistr 0.1 0.025
      xgPos = cumulative xgND (shotXG sd) 
  in xgPos

determineOnTheBallIntention :: (Monad m, Match m, Log m, Random m, Cache m SpaceCache) => OnTheBallCriteria -> Player -> m PlayerIntention
determineOnTheBallIntention otbc player = do
  ball <- gameBall
  time <- currentGameTime
  safePassingOptions <- safestPassingOptions player
  dribbleOptions <- desirableDribbleOptions player
  shotOptions <- filter (\so -> shotXG so >= 0.04) <$> desirableShotOptions player
  let combinedOptions = fmap ShotOption shotOptions ++ fmap PassOption safePassingOptions ++ fmap DribbleOption dribbleOptions
      validOptions = filter (not . isNaN . onTheBallOptionDesirabilityCoeff) combinedOptions
      sortedOptions = sortOn (Data.Ord.Down . onTheBallOptionDesirabilityCoeff) validOptions

  
  chosenOption <- pickFrom $ cycle sortedOptions

  pure $ case chosenOption of
    DribbleOption dd -> DribbleIntention (locate2D ball) (dribbleDirection dd)
    PassOption pd    -> PassIntention (passTarget pd) (locate2D ball) (passBallVector pd) $ gameTimeAddSeconds time 0.35
    ShotOption sd    -> ShootIntention (shotTarget sd) (locate2D ball) (shotBallVector sd)
  where
    minPassSafety = fromMaybe 0 $ otbcMinimumPassSafetyCoeff otbc
    minPassDesirability = fromMaybe (-1) $ otbcMinimumPassDesirabilityCoeff otbc


pickFrom :: (Monad m, Match m, Log m, Random m) => [OnTheBallOption] -> m OnTheBallOption
pickFrom xs = picker $ take 100 $ cycle xs
  where
    picker [x] = pure x
    picker (x:opts) = do
      optionVal <- randomRange 0 1
      if optionVal <= onTheBallOptionDesirabilityCoeff x then
        pure x
      else
        picker opts
    picker _ = error "Impossible"



