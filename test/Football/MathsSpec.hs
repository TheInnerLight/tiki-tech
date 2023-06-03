module Football.MathsSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Maths
import Linear (V3(V3))

(@?~=) :: (Ord a, Fractional a) => a -> a -> Assertion
(@?~=) a b =
  abs (a-b) `compare` 1e-6 @?= LT


mathsTests :: TestTree
mathsTests = testGroup "MathsSpec tests"
  [ testCase "Objects moving away should have closest intercept at initial point" $ do
      let v1p = V3 0 0 0
          v1m = V3 1 0 0
          v2p = V3 1 1 0
          v2m = V3 0.707 0.707 0

      let (t, p1, p2) = distanceAndClosestInterceptsWithinTimeStep (1.0) (v1p, v1m) (v2p, v2m)

      t @?~= 1.41421356
      p1 @?= v1p
      p2 @?= v2p
  , testCase "Objects moving toward should have closer intercept" $ do
      let v1p = V3 0 0 0
          v1m = V3 1 0 0
          v2p = V3 1 (-1) 0
          v2m = V3 (-0.707) 0.707 0

      let (t, p1, p2) = distanceAndClosestInterceptsWithinTimeStep (1.0) (v1p, v1m) (v2p, v2m)

      t @?~= 0.414364574
      p1 @?= V3 0.0 0.0 0.0
      p2 @?= V3 0.29300000000000004 (-0.29300000000000004) 0.0
  , testCase "Objects moving away should have closer intercept if time step is backward" $ do
      let v1p = V3 0 0 0
          v1m = V3 1 0 0
          v2p = V3 1 1 0
          v2m = V3 0.707 0.707 0

      let (t, p1, p2) = distanceAndClosestInterceptsWithinTimeStep (-1.0) (v1p, v1m) (v2p, v2m)
      t @?~= 0.414364574
      p1 @?= V3 0.0 0.0 0.0
      p2 @?= V3 0.29300000000000004 0.29300000000000004 0.0
  , testCase "Objects moving toward should have closest intercept at initial point if time step is backward" $ do
      let v1p = V3 0 0 0
          v1m = V3 1 0 0
          v2p = V3 1 (-1) 0
          v2m = V3 (-0.707) 0.707 0

      let (t, p1, p2) = distanceAndClosestInterceptsWithinTimeStep (-1.0) (v1p, v1m) (v2p, v2m)

      t @?~= 1.41421356
      p1 @?= v1p
      p2 @?= v2p
  ]
