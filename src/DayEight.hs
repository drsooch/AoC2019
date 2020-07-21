{-# LANGUAGE OverloadedStrings #-}
module DayEight (answersD8, parse, part1) where

import           Data.List
import           Data.List.Split (chunksOf)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

answersD8 :: IO ()
answersD8 = do
  sifImg <- parse
  print $ "Part One: " ++ show (part1 sifImg)
  mapM_ print (part2 sifImg)

width, height :: Int
width = 25
height = 6

layers :: Int -> Int -> [Int] -> [[Int]]
layers w h = chunksOf (w * h)

layers' :: [Int] -> [[Int]]
layers' = layers width height


countZeroes' :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
countZeroes' (idx, num, curr) xs = (idx', num', curr + 1)
  where
    numZ = (length . filter (== 0)) xs
    num' = min numZ num
    idx' = if numZ < num then curr else idx

countZeroes :: [Int] -> [Int] -> [Int]
countZeroes acc xs = if (count xs < count acc) then xs else acc
  where
    count = length . filter (== 0)

-- 2375 -> answer
part1 :: [Int] -> Int
part1 =
  product
  . (map length)
  . group
  . sort
  . filter (/= 0)
  . foldl1 countZeroes
  . layers'

-- RKHRY -> answer
part2 :: [Int] -> [[T.Text]]
part2 =
  chunksOf width
  . map transform
  . concatMap (take 1)
  . map (dropWhile (== 2))
  . transpose
  . layers'
  where
    transform x = if x == 1 then "X" else " "

parse :: IO [Int]
parse = do
  contents <- TIO.readFile "input/dayEight.txt"
  return $ (map (read . T.unpack) . init . T.chunksOf 1) contents
