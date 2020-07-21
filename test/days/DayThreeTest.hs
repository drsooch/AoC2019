module DayThreeTest (test_D3) where

import Test.HUnit
import DayThree

test_D3 :: Test
test_D3 = TestList [ test_D3P1
                   , test_D3P2
                   ]

test_D3P1 :: Test
test_D3P1 = TestCase $ do
  (s1, s2) <- parse
  let (p1, _) = solve s1 s2
  assertEqual "Failed Day Three - Part One" 731 p1

test_D3P2 :: Test
test_D3P2 = TestCase $ do
  (s1, s2) <- parse
  let (_, p2) = solve s1 s2
  assertEqual "Failed Day Three - Part Two" 5672 p2
