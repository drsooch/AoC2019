module DayTen (answersD10) where

import           Control.Monad.RWS.Strict
import           Data.List                (delete, elemIndices)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Tuple               (swap)
import qualified System.IO                as IO

type Coord = (Double, Double)
type CoordMap = Map Coord (Set Double)
type AngleMap = Map Double (Set (Double, Coord))

type LaserS = RWS [Int] [Coord] Laser

data Laser = L { numDest :: Int      -- Number of asteroids destroyed
               , am      :: AngleMap -- map of angles to coords
               , currAng :: Double   -- current angle of the laser
               , equal   :: Bool     -- do we want to use lookupGE or lookupGT
               , base    :: Coord    -- Base coordinate
               } deriving (Show)

-- Parsing in Strings for this day as Text does not have elemIndices
answersD10 :: IO ()
answersD10 = do
  handle <- IO.openFile "input/dayTen.txt" IO.ReadMode
  contents <- IO.hGetContents handle
  let asteroids = parseToCoordList $ lines contents
  let (p1, ans) = part1 asteroids
  print $ "Part One " ++ (show p1) ++ ": " ++ (show ans)
  let (_, _, destroyed) = part2 p1 asteroids
  print $ "Part Two: " ++ (show $ destroyed !! 199)
  print $ map (destroyed !!) [0, 1, 2, 3, 9, 19, 49, 99, 199, 200, 289]
  IO.hClose handle
  return ()


-- create a list of coordinates
parseToCoordList :: [String] -> [Coord]
parseToCoordList xs =
  map (toD . itc)      -- Map int to coords onto each asteroid index and transform to Double
  $ elemIndices '#'    -- Find each asteroid's index
  $ concat xs          -- Concat the input so we can remove new lines
  where
    width = length (xs !! 0)
    itc n = swap $ divMod n width  -- The coordinate system is flipped
    toD (x, y) = (fromIntegral x, fromIntegral y)

-- computes the angle from one coordinate to another
-- Bounds are [-pi, pi], for part 2 we adjust this range from [0, 2pi]
angle :: Coord -> Coord -> Double
angle (x1, y1) (x2, y2) = pi + atan2 (y1 - y2) (x1 - x2)


-- creates a laser
createLaser :: Coord -> [Coord] -> Laser
createLaser p ps = L {numDest = 0, am = angM, currAng = (3.0 * pi) / 2.0, equal = True, base = p}
  where
    ps' = map create $ delete p ps                           -- maps the create function to the coordinate list
    create c = (angle p c, distance p c)                     -- generates an angle to the coord, and a tuple of the distance to base and the coord
    insertP m (ang, point) = M.insertWith (++) ang [point] m -- insertion function into the map
    angM = M.map (S.fromList) $ foldl insertP M.empty ps'    -- instead of generating the set from the start, i went the lazy route

-- classic distance function except it returns the coordinate that produced it
distance :: Coord -> Coord -> (Double, Coord)
distance (x1, y1) t@(x2, y2) = (sqrt $ (x1 - x2)**2 + (y1 - y2)**2, t)

-- wrapper to run the laser until finished
runDestroy :: LaserS ()
runDestroy = do
  laser <- get
  coord <- destroy
  tell [coord]
  if M.null (am laser)
    then return ()
  else runDestroy

-- awfully imperative :-/
-- destroys the current visible asteroid
-- rotates the laser
destroy :: LaserS Coord
destroy = do
  laser <- get
  -- if we've done a lookup where there are more than one elements
  -- we don't want to search same list again
  let amLookup = if equal laser then M.lookupGE else M.lookupGT
  -- this catches the case where we need wrap around our angle i.e. 2pi -> 0
  let lkup = amLookup (currAng laser) (am laser)
  -- get the angle found and the Set of coords
  let (ang, coords) = case lkup of
                        Nothing -> fromJust $ M.lookupGE 0.0 (am laser)
                        Just x  -> x
  -- find and delete the closest coordinate
  let p@(_, toDest) = foldl1 (\acc test -> if (fst acc) > (fst test) then test else acc) coords
  let coords' = S.delete p coords
  -- if we've deleted the last coordinate then just delete the key/val pair otherwise update set
  -- the boolean doesn't matter except in the else clause
  let (equal', am') = if S.null coords' then (True, M.delete ang (am laser)) else (False, M.insert ang coords' (am laser))
  put laser {numDest = (numDest laser) + 1, am = am', currAng = ang, equal = equal'}
  return toDest

-- for each coordinate in the list, apply the angle formula from the current point
-- to the rest of the list
-- This will remove any points that are on the same line (except 1)
-- giving a Set of total number of visible asteroids
-- fold over all the values in the map to get the maximum size and its location
-- 247 -> answer
part1 :: [Coord] -> (Coord, Int)
part1 cm =
  foldl1 maximum'                    -- get the coordinate with the largest set
  $ map size                         -- Map the size function to the tuple of (coord, set)
  $ M.toList                         -- Turn the map into a list
  $ foldl (flip insertP) M.empty cm  -- fold over the list of coords, creating a map of a coordinate to a set of angles
  where
    cmA p1 = S.fromList $ map (angle p1) cm   -- create a set of angles from the point supplied
    insertP p = M.insert p (cmA p)            -- insert a coordinate and its angles into the map
    size (p, s) = (p, S.size s)               -- map set size from list
    maximum' acc@(_, s1) curr@(_, s2) = if s1 > s2 then acc else curr  -- maximum function on two tuples

-- runs the laser
-- (19, 19) -> 1919 -> answer
part2 :: Coord -> [Coord] -> ((), Laser, [Coord])
part2 p angM = runRWS runDestroy [] (createLaser p angM)
