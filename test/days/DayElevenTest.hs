module DayElevenTest (test_D11) where

import           DayEleven
import           Test.HUnit

test_D11 :: Test
test_D11 = TestList [ test_D11P1
                    , test_D11P2
                    ]

test_D11P1 :: Test
test_D11P1 = TestCase $ do
  inputs <- parse
  assertEqual "Failed Day Eleven - Part One" 2339 (part1 inputs)

test_D11P2 :: Test
test_D11P2 = TestCase $ do
  print "Visual Inspection Needed Day Eleven - Part Two"
