{-# LANGUAGE OverloadedStrings #-}

module DaySix (answersD6, parse, part1, part2) where


import           Data.Map        (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

type Orbits = T.Text
type System = Map T.Text Orbits

answersD6 :: IO ()
answersD6 = do
  contents <- parse
  print $ "Part One: " ++ show (part1 contents)
  print $ "Part Two: " ++ show (part2 contents)

-- parse the input and swap value with key for use with M.fromList
splitOrbit :: T.Text -> (T.Text, T.Text)
splitOrbit orbit = (orbiter, center)
  where
  [center, orbiter] = T.splitOn ")" orbit

-- recursively traverse a full orbit to COM node
countOrbits :: System -> T.Text -> Int
countOrbits = go 0
  where
    go acc _ "COM" = acc
    go acc mp k    = go (acc + 1) mp (mp M.! k)

-- 315757 -> answer
part1 :: [(T.Text, T.Text)] -> Int
part1 o = sum $ map (countOrbits orbits) $ M.keys orbits
  where
    orbits = M.fromList o

youNode :: System -> T.Text
youNode = flip (M.!) "YOU"

sanNode :: System -> T.Text
sanNode = flip (M.!) "SAN"

-- Build a set of a location's Path to COM Node
traverseOrbit :: System -> T.Text -> Set T.Text
traverseOrbit = go S.empty
  where
    go s _ "COM" = s
    go s mp k    = go (S.insert k s) mp (mp M.! k)

-- Traverse the orbits of the YOU node to COM and SAN node to COM
-- these orbit paths are encoded as Sets
-- We need the total length between YOU and SAN
-- Union the two differences as each difference represents one half of a path to
-- the common ancestor node
part2 :: [(T.Text, T.Text)] -> Int
part2 o = S.size $ S.union (p1 S.\\ p2) (p2 S.\\ p1)
  where
    orbits = M.fromList o
    p1 = traverseOrbit orbits $ sanNode orbits
    p2 = traverseOrbit orbits $ youNode orbits

parse :: IO [(T.Text, T.Text)]
parse = do
  contents <- TIO.readFile "input/daySix.txt"
  return $ map splitOrbit $ T.lines contents
