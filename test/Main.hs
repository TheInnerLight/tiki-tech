module Main (main) where

import Test.Tasty
import Football.Understanding.SpaceSpec (spaceSpecTests)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Football.Behaviours.PassSpec (passTests)
import Football.MathsSpec (mathsTests)
import Football.Intentions.OnTheBallSpec (onTheBallTests)
import Football.Understanding.ExpectedGoalsSpec (expectedGoalsSpecTests)
import Football.Understanding.LineBreakingSpec (lineBreakingSpecTests)
import Football.Understanding.ShapeSpec (shapeSpecTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [shapeSpecTests, spaceSpecTests, passTests, mathsTests, onTheBallTests, expectedGoalsSpecTests, lineBreakingSpecTests]
