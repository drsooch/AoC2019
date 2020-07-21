module DayNine (answersD9, parse, part1, part2) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           IntCode

answersD9 :: IO ()
answersD9 = do
  values <- parse
  print $ "Part One: " ++ show (part1 values)
  print $ "Part Two: " ++ show (part2 values)

-- 3100786347 -> answer
part1 :: [Integer] -> Integer
part1 mem = last $ getMachineOutput mem [1] []

--  87023
part2 :: [Integer] -> Integer
part2 mem = last $ getMachineOutput mem [2] []

parse :: IO [Integer]
parse = do
  contents <- TIO.readFile "input/dayNine.txt"
  return $ map (read . T.unpack) $ T.splitOn (T.pack ",") contents
