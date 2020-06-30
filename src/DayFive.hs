module DayFive (answersD5) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           IntCode

answersD5 :: IO ()
answersD5 = do
  contents <- TIO.readFile "input/dayFive.txt"
  let values = map (read . T.unpack) $ T.splitOn (T.pack ",") contents :: [Integer]
  print $ "Part One: " ++ (show $ part1 values)
  print $ "Part Two: " ++ (show $ part2 values)


-- 10987514 -> answer
part1 :: [Integer] -> Integer
part1 mem = getMachineOutput mem [1]

-- 14195011 -> answer
part2 :: [Integer] -> Integer
part2 mem = getMachineOutput mem [5]

