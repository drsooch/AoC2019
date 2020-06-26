module DayThree (answersD3) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           Data.Text.IO    as TIO

data Dir = North
         | South
         | West
         | East
        deriving (Show)

type Coord = (Int, Int)
type Points = M.Map Coord Int -- Map a Coordinate to a step number

origin :: Coord
origin = (0, 0)

answersD3 :: IO ()
answersD3 = do
  contents <- TIO.readFile "input/dayThree.txt"
  let [segments1, segments2] = map (map T.unpack . T.splitOn (T.pack ",")) $ T.lines contents
  let (p1, p2) =  solve segments1 segments2
  print $ "Part 1: " ++ (show p1)
  print $ "Part 2: " ++ (show p2)

-- generates an X, Y coordinate and the total number of steps to that point
-- silently failed ex: "b - len - 1" should be "b - (len - 1)"
genCoords :: Coord -> (Int, Int) -> Dir -> [(Int, Int, Int)]
genCoords (x, y) (curr, len) d = case d of
  North -> zip3 (repeat x) (asc y) (asc curr)
  South -> zip3 (repeat x) (desc y) (asc curr)
  West  -> zip3 (desc x) (repeat y) (asc curr)
  East  -> zip3 (asc x) (repeat y) (asc curr)
  where
    desc b    = [b - len, b - (len - 1) .. b] -- descending list
    asc  c    = [c ..  (c + len)]             -- ascending list

-- Folds over the Input, keeping track of current position, number of steps taken
-- and creates a Map of all coordinates traversed
traverseWire :: Coord -> Int -> [String] -> Points -> Points
traverseWire (_, _) _ [] s = s
traverseWire (x, y) dist ((dir : steps) : rest) s = case dir of
  'U' -> traverseWire (x, y + steps') newDist rest (union North)
  'D' -> traverseWire (x, y - steps') newDist rest (union South)
  'L' -> traverseWire (x - steps', y) newDist rest (union West)
  'R' -> traverseWire (x + steps', y) newDist rest (union East)
  _   -> undefined  -- Shouldn't reach
  where
    steps' = read steps :: Int                              -- Converts String -> Int
    newDist = dist + steps'                                 -- Total distance traveled
    union d  = M.union s $ M.fromList $ create d            -- Unions two maps together defaults to the left map which means any coords that are duplicates we default to the first pass
    create d = createPoints (x, y) (dist, steps') d      -- helper function to create new segment

createPoints :: Coord -> (Int, Int) -> Dir -> [(Coord, Int)]
createPoints (x, y) (curr, steps) dir = map transform $ genCoords (x, y) (curr, steps) dir
  where
    transform (a, b, c) = ((a, b), c)

-- folds over the Map applying ManhattanDist and getting the min
-- 731 -> answer
part1 :: Points -> Int
part1 = M.foldrWithKey (\k _ m -> if manhattanDist k < m then manhattanDist k else m) (maxBound :: Int)

-- get the minimum value in the map
part2 :: Points -> Int
part2 = M.foldl min (maxBound :: Int)

-- wraps up part1 and part2's answers into a Tuple
solve :: [String] -> [String] -> (Int, Int)
solve s1 s2 = (part1 intersection, part2 intersection)
  where
    wire1     = traverseWire origin 0 s1 M.empty
    wire2     = traverseWire origin 0 s2 M.empty
    -- delete the Origin point which is usually guaranteed to be there
    -- combine the intersection adding the two length values to get a total distance
    intersection = M.delete origin $ M.intersectionWith (+) wire1 wire2

manhattanDist :: Coord -> Int
manhattanDist (x, y)  = abs x + abs y

