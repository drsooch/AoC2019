module DayTenTest (test_D10) where

import           DayTen
import           Test.HUnit

test_D10 :: Test
test_D10 = TestList [ test_D10P1
                    , test_D10P2
                    ]

test_D10P1 :: Test
test_D10P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Ten - Part One" 247 (snd $ part1 inputs)

test_D10P2 :: Test
test_D10P2 = TestCase $ do
  inputs <- parse
  let p1 = fst $ part1 inputs
  assertEqual "Failed Day Ten - Part Two" (19, 19) (part2 p1 inputs !! 199)
