module DayFive (answersD5, parse, part1, part2) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           IntCode

answersD5 :: IO ()
answersD5 = do
  values <- parse
  print $ "Part One: " ++ show (part1 values)
  print $ "Part Two: " ++ show (part2 values)


-- 10987514 -> answer
part1 :: [Integer] -> Integer
part1 mem = last $ getMachineOutput mem [1] []

-- 14195011 -> answer
part2 :: [Integer] -> Integer
part2 mem = last $ getMachineOutput mem [5] []

parse :: IO [Integer]
parse = do
  contents <- TIO.readFile "input/dayFive.txt"
  return $ map (read . T.unpack) $ T.splitOn (T.pack ",") contents
