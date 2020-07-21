module DayNineTest (test_D9) where

import           DayNine
import           Test.HUnit

test_D9 :: Test
test_D9 = TestList [ test_D9P1
                   , test_D9P2
                   ]

test_D9P1 :: Test
test_D9P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Nine - Part One" 3100786347 (part1 inputs)

test_D9P2 :: Test
test_D9P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Nine - Part Two" 87023 (part2 inputs)
