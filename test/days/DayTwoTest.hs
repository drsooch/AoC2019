module DayTwoTest (test_D2) where

import Test.HUnit
import DayTwo

test_D2 :: Test
test_D2 = TestList [ test_D2P1
                   , test_D2P2
                   ]

test_D2P1 :: Test
test_D2P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Two - Part One" 3101844 (part1 inputs)


test_D2P2 :: Test
test_D2P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Two - Part Two" (84, 78) (part2 nounVerbPairs inputs)
