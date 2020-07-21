module DayThirteenTest (test_D13) where

import           DayThirteen
import           Test.HUnit

test_D13 :: Test
test_D13 = TestList [ test_D13P1
                    , test_D13P2
                    ]

test_D13P1 :: Test
test_D13P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Thirteen - Part One" (part1 inputs) 376

test_D13P2 :: Test
test_D13P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Thirteen - Part Two" (part2 $ 2 : tail inputs) (Just 18509)
