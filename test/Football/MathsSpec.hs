module Football.MathsSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Football.Maths
import Linear (V3(V3), V2 (V2))
import Football.Maths (lineLineIntersection)

(@?~=) :: (Ord a, Fractional a) => a -> a -> Assertion
(@?~=) a b =
  abs (a-b) `compare` 1e-6 @?= LT

mathsTests :: TestTree
mathsTests = testGroup "MathsSpec tests"
  [ distanceAndClosestInterceptsTests
  , lineLineIntersectionTests
  ]

distanceAndClosestInterceptsTests :: TestTree
distanceAndClosestInterceptsTests = testGroup "Distance and Closest Intercepts tests"
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



lineLineIntersectionTests :: TestTree
lineLineIntersectionTests = testGroup "Line Line Intersection tests"
  [ testCase "Lines don't intersect" $ do
      let l11 = V2 0 1
      let l12 = V2 0 3
      let l21 = V2 1 0
      let l22 = V2 3 0

      lineLineIntersection (l11, l12) (l21, l22) @?= Nothing
  , testCase "Lines intersect" $ do
      let l11 = V2 1 1
      let l12 = V2 0 0
      let l21 = V2 1 0
      let l22 = V2 0 1

      lineLineIntersection (l11, l12) (l21, l22) @?= Just (V2 0.5 0.5)
  , testCase "Perpedicular lines intersect at start" $ do
      let l11 = V2 0 0
      let l12 = V2 2 0
      let l21 = V2 0 0
      let l22 = V2 0 2

      lineLineIntersection (l11, l12) (l21, l22) @?= Just (V2 0 0)
    , testCase "Lines intersect at end" $ do
      let l11 = V2 0 0
      let l12 = V2 2 0
      let l21 = V2 3 3
      let l22 = V2 2 0

      lineLineIntersection (l11, l12) (l21, l22) @?= Just (V2 2.0 0.0)
  ]


