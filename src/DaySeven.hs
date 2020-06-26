module DaySeven (answersD7) where

import           Data.Foldable (foldl')
import           Data.List     (permutations)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           IntCode

answersD7 :: IO ()
answersD7 = do
  contents <- TIO.readFile "input/daySeven.txt"
  let values = map (read . T.unpack) $ T.splitOn (T.pack ",") contents :: [Int]
  print $ "Part One: " ++ (show $ part1 values)
  print $ "Part Two: " ++ (show $ part2 values)


-- 13848 -> answer
part1 :: [Int] -> Int
part1 mem = maximum $ map (runChain mem) perms
  where
    perms = permutations [0 .. 4]

-- 12932154 -> answer
part2 :: [Int] -> Int
part2 mem = maximum $ map (runFeedbackLoop mem) perms
  where
    perms = permutations [5 .. 9]

