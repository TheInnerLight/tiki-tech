{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Football.Understanding.ExpectedGoalsSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (AttackingLeftToRight, AttackingRightToLeft))
import Football.Types
import Football.Understanding.Space (pitchHorizontalZone)
import Football.Understanding.Space.Data
import Football.Understanding.Space.Data (HorizontalZone(CentreHZ))
import Football.Understanding.ExpectedGoals (locationXG)
import Linear (V2(V2))

newtype TestMLR a = TestM {unTestMLR :: IO a}
  deriving (Functor, Applicative, Monad)

instance Match TestMLR where
  attackingDirection Team1 = pure AttackingLeftToRight
  attackingDirection Team2 = pure AttackingRightToLeft
  pitch = pure $ Pitch 105 68

runTestMLR :: TestMLR a -> IO a
runTestMLR = unTestMLR

expectedGoalsSpecTests :: TestTree
expectedGoalsSpecTests = testGroup "ExpectedGoals tests"
  [ testCase "XG for team 1 near team 2 goal is higher than team 2 xg" $ do
      let location :: V2 Double = V2 50 0
      result <- runTestMLR $ locationXG Team1 location
      result2 <- runTestMLR $ locationXG Team2 location
      result `compare` result2 @?= GT
  ,  testCase "XG for team 2 near team 1 goal is higher than team 1 xg" $ do
      let location :: V2 Double = V2 (-50) 0
      result <- runTestMLR $ locationXG Team1 location
      result2 <- runTestMLR $ locationXG Team2 location
      result2 `compare` result @?= GT
  ]