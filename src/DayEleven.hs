{-# LANGUAGE OverloadedStrings #-}
module DayEleven (answersD11) where

import           Control.Monad.RWS.Strict
import           Data.List                (transpose)
import qualified Data.List.Split          as LS
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           IntCode

type Coord = (Int, Int)
type Hull = Map Coord Color

data Color = Black
           | White
           deriving (Show, Eq, Enum)

data Dir = North
         | South
         | East
         | West
         deriving (Show, Eq)

data Robot = Robot { hull    :: Hull
                   , painted :: Set Coord
                   , currLoc :: Coord
                   , im      :: IMachine
                   , dir     :: Dir
                   , onPaint :: Bool
                   } deriving (Show)


answersD11 :: IO ()
answersD11 = do
  contents <- map (read . T.unpack) . T.splitOn "," <$> TIO.readFile "input/dayEleven.txt" :: IO [Integer]
  print $ "Part One: " ++ (show $ part1 contents)
  putStrLn "Part Two"
  mapM_ putStrLn $ part2 contents

-- 2339 -> answer
part1 :: [Integer] -> Int
part1 = S.size . painted . runRobot . mkRobot

-- PGUEPLPR -> answer
part2 :: [Integer] -> [String]
part2 = hullToChar . runRobot . mkRobotPart2

-- create a Robot
mkRobot :: [Integer] -> Robot
mkRobot mem = Robot { hull = M.empty
                    , painted = S.empty
                    , currLoc = (0, 0)
                    , im = mkIM mem
                    , dir = North
                    , onPaint = True
                    }

-- change the starting square to white
mkRobotPart2 :: [Integer] -> Robot
mkRobotPart2 mem = (mkRobot mem) {hull = M.singleton (0, 0) White}

-- Simple "loop" to run a robot
-- the current coordinate color is always provided as input
-- we pause if we encounter an incoming LoadVal to retrieve any
-- outputs generated and paint/move
runRobot :: Robot -> Robot
runRobot r = do
  let currCol = fromColor $ coordToColor r
  let (op, im', out) = runRWS (runMachine [LoadVal, Halt]) [currCol] (im r)
  if op == Halt
    then r
    else runRobot $ movePaint out $ updateIM im' r


-- simple recursive function to consume all output
-- painting or moving depending on the bool flag onPaint
movePaint :: [Integer] -> Robot -> Robot
movePaint (x:xs) r = case (onPaint r) of
                       True  -> movePaint xs $ paint x r
                       False -> movePaint xs $ turn x r
movePaint [] r      = r

-- paint a cell and update painted Set and Hull
-- flips the onPaint flag as well
paint :: Integer -> Robot -> Robot
paint col r = notPaint $ r {hull = hull', painted = painted'}
  where
    hull' = M.insert (currLoc r) (toColor col) (hull r)
    painted' = S.insert (currLoc r) (painted r)

-- turn moves a robot based on current direction and turn type
-- 0 = Left
-- 1 = Right
-- flips the onPaint flag
turn :: Integer -> Robot -> Robot
turn 0 r = notPaint $ case dir r of
             North -> r { currLoc = addCoords (currLoc r) (-1, 0), dir = West}
             South -> r { currLoc = addCoords (currLoc r) (1, 0), dir = East}
             East  -> r { currLoc = addCoords (currLoc r) (0, 1), dir = North}
             West  -> r { currLoc = addCoords (currLoc r) (0, -1), dir = South}
turn 1 r = notPaint $ case dir r of
             North -> r { currLoc = addCoords (currLoc r) (1, 0), dir = East}
             South -> r { currLoc = addCoords (currLoc r) (-1, 0), dir = West}
             East  -> r { currLoc = addCoords (currLoc r) (0, -1), dir = South}
             West  -> r { currLoc = addCoords (currLoc r) (0, 1), dir = North}

-- helper to insert new IMachine
-- and reset the input index as we always have only one input
updateIM :: IMachine -> Robot -> Robot
updateIM i r = r {im = im'}
  where
    im' = i {inpIx_ = 0}

-- helper to flip onPaint flag
notPaint :: Robot -> Robot
notPaint r = r {onPaint = not (onPaint r)}

-- integer to color
toColor :: Integer -> Color
toColor = toEnum . fromIntegral

-- color to integer
fromColor :: Color -> Integer
fromColor = fromIntegral . fromEnum

-- take current location and add a turn to it
addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- get the current location color
coordToColor :: Robot -> Color
coordToColor r = M.findWithDefault Black (currLoc r) (hull r)

-- translate a color into a char
hullToChar :: Robot -> [String]
hullToChar = map (map toChar) . splitHull
  where
    toChar Black = ' '
    toChar White = '█'

-- determine the size of the hull and create a list of each row
-- transforming each coord into a color
splitHull :: Robot -> [[Color]]
splitHull r = map (map (\c -> M.findWithDefault Black c (hull r)))
  $ cells
  where
    minX = minimum $ map fst $ M.keys (hull r)
    maxX = maximum $ map fst $ M.keys (hull r)
    minY = minimum $ map snd $ M.keys (hull r)
    maxY = maximum $ map snd $ M.keys (hull r)
    cells = [makeRow minX maxX y | y <- reverse [minY .. maxY]]

-- helper to make a row of coords
makeRow :: Int -> Int -> Int -> [Coord]
makeRow minX maxX y = [(x, y) | x <- [minX .. maxX]]
