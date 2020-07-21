module DaySixTest (test_D6) where

import Test.HUnit
import DaySix

test_D6 :: Test
test_D6 = TestList [ test_D6P1
                   , test_D6P2
                   ]

test_D6P1 :: Test
test_D6P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Six - Part One" 315757 (part1 inputs)

test_D6P2 :: Test
test_D6P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Six - Part Two" 481 (part2 inputs)
