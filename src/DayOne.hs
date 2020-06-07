module DayOne where

fuel :: Int -> [Int]
fuel n = result : takeWhile (>0) (fuel result)
  where
    result = fuel' n

fuel' :: Int -> Int
fuel' n = subtract 2 $ div n 3

main :: IO ()
main = do
  contents <- readFile "../input/dayOne.txt"
  print $ sum $ concatMap fuel $ map (read :: String -> Int) $ lines contents
