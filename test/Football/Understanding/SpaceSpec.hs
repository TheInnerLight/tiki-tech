{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Football.Understanding.SpaceSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Match (Match(..), AttackingDirection (AttackingLeftToRight))
import Football.Types
import Football.Understanding.Space (pitchHorizontalZone)
import Football.Understanding.Space.Data

newtype TestMLR a = TestM {unTestMLR :: IO a}
  deriving (Functor, Applicative, Monad)

instance Match TestMLR where
  attackingDirection _ = pure AttackingLeftToRight

runTestMLR :: TestMLR a -> IO a
runTestMLR = unTestMLR

spaceSpecTests :: TestTree
spaceSpecTests = testGroup "SpaceSpec tests"
  [ testCase "Top of pitch is on left wing when attacking left to right" $ do
      result <- runTestMLR $ pitchHorizontalZone TeamId1 (0.0 :: Double, -30.0 :: Double)
      result @?= WingHZ LeftHalf
  , testCase "Middle of pitch is in centre when attacking left to right" $ do
      result <- runTestMLR $ pitchHorizontalZone TeamId1 (0.0 :: Double, 0.0 :: Double)
      result @?= CentreHZ
  , testCase "Bottom of pitch is on right wing when attacking left to right" $ do
      result <- runTestMLR $ pitchHorizontalZone TeamId1 (0.0 :: Double, 30.0 :: Double)
      result @?= WingHZ RightHalf
  ]

