module DaySevenTest (test_D7) where

import Test.HUnit
import DaySeven

test_D7 :: Test
test_D7 = TestList [ test_D7P1
                   , test_D7P2
                   ]

test_D7P1 :: Test
test_D7P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Seven - Part One" 13848 (part1 inputs)

test_D7P2 :: Test
test_D7P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Seven - Part Two" 12932154 (part2 inputs)
