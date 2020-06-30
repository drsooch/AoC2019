module DayNine (answersD9) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           IntCode

answersD9 :: IO ()
answersD9 = do
  contents <- TIO.readFile "input/dayNine.txt"
  let values = map (read . T.unpack) $ T.splitOn (T.pack ",") contents :: [Integer]
  print $ part1 values
  print $ part2 values

-- 3100786347 -> answer
part1 :: [Integer] -> [Integer]
part1 mem = getMachineOutput' mem [1]

--  87023
part2 :: [Integer] -> [Integer]
part2 mem = getMachineOutput' mem [2]
