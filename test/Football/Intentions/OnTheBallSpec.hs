{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Football.Intentions.OnTheBallSpec where
import Test.Tasty
import Football.Behaviours.Pass (PassDesirability(..))
import Football.Types (PassTarget(SpaceTarget))
import Linear (V3(V3), V2 (V2), Metric (dot))
import Football.Intentions.OnTheBall (OnTheBallOption(PassOption), onTheBallOptionDesirabilityCoeff)
import Test.Tasty.HUnit

(@?~=) :: (Ord a, Fractional a) => a -> a -> Assertion
(@?~=) a b =
  abs (a-b) `compare` 1e-6 @?= LT

onTheBallTests :: TestTree
onTheBallTests = testGroup "Desirability calculation tests"
    [ testCase "A very unreliable pass should have low desirability" $ do
        let unreliablePass = PassDesirability
              { passTarget = SpaceTarget (V2 0 0)
              , passBallVector = V3 1 1 1
              , passOppositionInterceptionDistance = 1
              , passTeammateReceptionDistance = 20
              , passSafetyCoeff = 7.59750720556321e-2
              , passXGAdded = 1.0515178437939708e-10
              , passOppositionXGAdded = -3.84819110288071e-2
              }
        
        let coeff = onTheBallOptionDesirabilityCoeff $ PassOption unreliablePass

        coeff @?= 6.538370545137032e-7
    , testCase "A very reliable pass should have high desirability" $ do
        let reliablePass = PassDesirability
              { passTarget = SpaceTarget (V2 0 0)
              , passBallVector = V3 1 1 1
              , passOppositionInterceptionDistance = 20
              , passTeammateReceptionDistance = 1
              , passSafetyCoeff = 0.9
              , passXGAdded = 1.0515178437939708e-10
              , passOppositionXGAdded = -3.84819110288071e-2
              }
        
        let coeff = onTheBallOptionDesirabilityCoeff $ PassOption reliablePass

        coeff @?= 0.8661346500472701
    , testCase "A very reliable pass with high XG should have very high desirability" $ do
        let reliablePass = PassDesirability
              { passTarget = SpaceTarget (V2 0 0)
              , passBallVector = V3 1 1 1
              , passOppositionInterceptionDistance = 20
              , passTeammateReceptionDistance = 1
              , passSafetyCoeff = 0.9
              , passXGAdded = 0.07
              , passOppositionXGAdded = -3.84819110288071e-2
              }
        
        let coeff = onTheBallOptionDesirabilityCoeff $ PassOption reliablePass

        coeff @?= 0.999999869586897
    , testCase "A very unreliable pass with high XG should have medium desirability" $ do
        let reliablePass = PassDesirability
              { passTarget = SpaceTarget (V2 0 0)
              , passBallVector = V3 1 1 1
              , passOppositionInterceptionDistance = 20
              , passTeammateReceptionDistance = 1
              , passSafetyCoeff = 0.1
              , passXGAdded = 0.09
              , passOppositionXGAdded = -3.84819110288071e-2
              }
        
        let coeff = onTheBallOptionDesirabilityCoeff $ PassOption reliablePass

        coeff @?= 0.7022744551330404


    ]
