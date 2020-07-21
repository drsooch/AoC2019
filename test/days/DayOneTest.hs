module DayOneTest (test_D1) where

import Test.HUnit
import DayOne

test_D1 :: Test
test_D1 = TestList [ test_D1P1
          , test_D1P2
          ]

test_D1P1 :: Test
test_D1P1 = TestCase $ do
  inputs <- parseD1
  assertEqual "Failed Day One - Part One" 3325156 (part1 inputs)

test_D1P2 :: Test
test_D1P2 = TestCase $ do
  inputs <- parseD1
  assertEqual "Failed Day One - Part Two" 4984866 (part2 inputs)
