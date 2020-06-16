module DayOne (answersD1) where

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
  contents <- readFile "input/dayOne.txt"
  let modules = map (read :: String -> Int) $ lines contents
  print $ part1 modules
  print $ part2 modules
