module DayFour (answersD4) where
{-

 Part One:
   - It is a six-digit number.
   - The value is within the range given in your puzzle input.
   - Two adjacent digits are the same (like 22 in 122345).
   - Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

RANGE: 240298 - 784956
-}

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

minVal :: Int
minVal = 240298

maxVal :: Int
maxVal = 784956

-- Using list comprehensions to create digits in ascending order in our range
-- This is guaranteed to be in our range and increasing
-- Thanks to: https://work.njae.me.uk/2019/12/04/advent-of-code-2019-day-4/
-- Using his work kinda defeats the whole purpose but when I originally worked on this
-- months ago I thought this was brilliant
range :: [(Int, Int, Int, Int, Int, Int)]
range = [(d1, d2, d3, d4, d5, d6)
        | d1 <- [2 .. 7]
        , d2 <- [d1 .. 9]
        , d3 <- [d2 .. 9]
        , d4 <- [d3 .. 9]
        , d5 <- [d4 .. 9]
        , d6 <- [d5 .. 9]
        ]

-- transform a tuple of digits to a number
toNum :: (Int, Int, Int, Int, Int, Int) -> Int
toNum (d1, d2, d3, d4, d5, d6) =
  10^5 * d1 + 10^4 * d2 + 10^3 * d3 + 10^2 * d4 + 10 * d5 + d6

-- make sure value is in the range provided
inRange :: (Int, Int, Int, Int, Int, Int) -> Bool
inRange n = num >= minVal && num <= maxVal
  where
    num = toNum n

-- checks atleast two digits repeat
repeating :: (Int, Int, Int, Int, Int, Int) -> Bool
repeating (d1, d2, d3, d4, d5, d6) =
  d1 == d2 || d2 == d3 || d3 == d4 || d4 == d5 || d5 == d6

-- part two only wants doubles and no more
-- check the preceding and succeeding digits of the two being tested
doubles :: (Int, Int, Int, Int, Int, Int) -> Bool
doubles (d1, d2, d3, d4, d5, d6) =
  (d1 == d2 && d3 /= d2)
  || (d2 == d3 && d4 /= d3 && d1 /= d2)
  || (d3 == d4 && d5 /= d4 && d2 /= d3)
  || (d4 == d5 && d6 /= d5 && d3 /= d4)
  || ( d5 == d6 && d4 /= d5)


-- 1150 -> answer
part1 :: [(Int, Int, Int, Int, Int, Int)] -> Int
part1 = length . filter (\n -> repeating n && inRange n)

-- 748 -> answer
part2 :: [(Int, Int, Int, Int, Int, Int)] -> Int
part2 = length . filter (\n -> doubles n && inRange n)

answersD4 :: IO ()
answersD4 = do
  print $ "Part One: " ++ (show (part1 range))
  print $ "Part Two: " ++ (show (part2 range))
