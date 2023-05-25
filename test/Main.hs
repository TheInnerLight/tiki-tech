module Main (main) where

import Test.Tasty
import Football.Understanding.SpaceSpec (spaceSpecTests)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [spaceSpecTests]
