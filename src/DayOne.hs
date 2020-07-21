module DayOne (answersD1, parseD1, part1, part2) where

fuel :: Int -> [Int]
fuel n = result : takeWhile (>0) (fuel result)
  where
    result = fuel' n

fuel' :: Int -> Int
fuel' n = subtract 2 $ div n 3

part1 :: [Int] -> Int
part1 = sum . map fuel'

part2 :: [Int] -> Int
part2 = sum . concatMap fuel

answersD1 :: IO ()
answersD1 = do
  modules <- parseD1
  print $ "Part One: " ++ show (part1 modules)
  print $ "Part Two: " ++ show (part2 modules)

parseD1 :: IO [Int]
parseD1 = do
  contents <- readFile "input/dayOne.txt"
  return $ map read $ lines contents
