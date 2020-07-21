module DayTwo (answersD2, part1, part2, parse, nounVerbPairs) where

import           Control.Monad.State
import qualified Data.Text           as T
import           Data.Text.IO        as TIO
import           IntCodeS


answersD2 :: IO ()
answersD2 = do
  initIM <- parse
  let partOne = part1 initIM
  let partTwo = part2 nounVerbPairs initIM
  print $ "Part One: " ++ show partOne
  print $ "Part Two: " ++ show partTwo ++ " " ++ show (100 * fst partTwo + snd partTwo)

-- 3101844 -> answer
part1 :: IMachine -> Int
part1 initState = getMemValue 0 finalState
  where
    replaceState = injectValues [(1, 12), (2, 2)] initState
    finalState = execState runMachine replaceState

-- all possible values that we can insert into the machine for part 2
nounVerbPairs :: [(Int, Int)]
nounVerbPairs = [(x, y) | x <- [0..99], y <- [0..99]]

-- magic value to halt part 2
magicNumber :: Int
magicNumber = 19690720

-- (84, 78) -> 8478 -> answer
part2 :: [(Int, Int)] -> IMachine -> (Int, Int)
part2 [] _             = error "Can't find Magic Number"
part2 ((x, y) : xs) im = if getMemValue 0 finalState == magicNumber then (x, y) else part2 xs im
  where
    replaceState = injectValues [(1, x), (2, y)] im
    finalState = execState runMachine replaceState

parse :: IO IMachine
parse = do
  contents <- TIO.readFile "input/dayTwo.txt"
  return $ createIM $ map (read . T.unpack) $ T.splitOn (T.pack ",") contents
