module DayFourTest (test_D4) where

import           DayFour
import           Test.HUnit

test_D4 :: Test
test_D4 = TestList [ test_D4P1
                   , test_D4P2
                   ]

test_D4P1 :: Test
test_D4P1 = TestCase $ do
  assertEqual "Failed Day Four - Part One" 1150 (part1 range)

test_D4P2 :: Test
test_D4P2 = TestCase $ do
  assertEqual "Failed Day Four - Part Two" 748 (part2 range)
