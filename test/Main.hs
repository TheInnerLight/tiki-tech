module Main (main) where

import Test.Tasty
import Football.Understanding.SpaceSpec (spaceSpecTests)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Football.Behaviours.PassSpec (passTests)
import Football.MathsSpec (mathsTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [spaceSpecTests, passTests, mathsTests]
