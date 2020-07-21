module DayFiveTest (test_D5) where

import           DayFive
import           Test.HUnit

test_D5 :: Test
test_D5 = TestList [ test_D5P1
                   , test_D5P2
                   ]

test_D5P1 :: Test
test_D5P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Five - Part One" 10987514 (part1 inputs)

test_D5P2 :: Test
test_D5P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Five - Part Two" 14195011 (part2 inputs)
