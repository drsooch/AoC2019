module DayEightTest (test_D8) where

import           DayEight
import           Test.HUnit

test_D8 :: Test
test_D8 = TestList [ test_D8P1
                   , test_D8P2
                   ]

test_D8P1 :: Test
test_D8P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Eight - Part One" 2375 (part1 inputs)

test_D8P2 :: Test
test_D8P2 = TestCase $ do
  print "Visual Inspection needed for Day Eight - Part Two"
