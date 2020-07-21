module DayTwelveTest (test_D12) where

import           DayTwelve
import           Test.HUnit

test_D12 :: Test
test_D12 = TestList [ test_D12P1
                    , test_D12P2
                    ]

test_D12P1 :: Test
test_D12P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Twelve - Part One" 6423 (part1 inputs)

test_D12P2 :: Test
test_D12P2 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Twelve - Part Two" 327636285682704 (part2 inputs)
