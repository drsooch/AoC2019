{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module DayTwelve (answersD12, parse, part1, part2) where

import           Data.List    (foldl')
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Linear       (V1 (..), V3 (..))
import qualified Linear       as L

type Moons = [Moon]
type Vector = V3 Int
data Moon = Moon { pos_ :: Vector
                 , vel_ :: Vector
                 } deriving (Show, Eq)

data Coord = X | Y | Z deriving (Show, Eq, Enum)


answersD12 :: IO ()
answersD12 = do
  moons <- parse
  print $ "Part One: " ++ show (part1 moons)
  print $ "Part Two: " ++ show (part2 moons)


-- 6423 -> answer
part1 :: [[Int]] -> Int
part1 = sum . map totalEnergy . head . drop 1000 . simulate . mkMoons

-- 327636285682704 -> answer
part2 :: [[Int]] -> Integer
part2 m = foldl' lcm 1 $ map (+2) [moonsX, moonsY, moonsZ]
  where
    sim = simulate
    moonsX = search X $ mkMoonsP2 X m
    moonsY = search Y $ mkMoonsP2 Y m
    moonsZ = search Z $ mkMoonsP2 Z m

-- one step of the simulation
-- updates the velocity of each moon,
-- then applies the velocity to the position of each
timestep :: Moons -> Moons
timestep = applyVel . updateVel

-- iterates the timestep over the Moons infinitely
simulate :: Moons -> [Moons]
simulate = iterate timestep

-- makes a moon based on parsed location
-- default velocities are 0
mkMoon :: [Int] -> Moon
mkMoon (x:y:z:_) = Moon {pos_ = V3 x y z, vel_ = V3 0 0 0}

mkMoonP2 :: [Int] -> Coord -> Moon
mkMoonP2 (x:_:_:_) X = Moon {pos_ = V3 x 0 0, vel_ = V3 0 0 0}
mkMoonP2 (_:y:_:_) Y = Moon {pos_ = V3 0 y 0, vel_ = V3 0 0 0}
mkMoonP2 (_:_:z:_) Z = Moon {pos_ = V3 0 0 z, vel_ = V3 0 0 0}

-- Make the 4 moons
mkMoons :: [[Int]] -> Moons
mkMoons = map mkMoon

mkMoonsP2 :: Coord -> [[Int]] -> Moons
mkMoonsP2 c = map (flip mkMoonP2 c)

-- applies gravity between moon 1 and 2
-- and updates velocity of moon 1 accordingly
applyGrav :: Moon -> Moon -> Moon
applyGrav (Moon p1 v1) (Moon p2 _) = Moon p1 (v1 L.^+^ v1')
  where
    cmp EQ = 0
    cmp GT = -1
    cmp LT = 1
    v1' = cmp <$> L.liftI2 compare p1 p2

-- take each moon in list and fold it over the same list
-- with the applyGrav function
-- updates the velocity of each moon
updateVel :: Moons -> Moons
updateVel ms = map (\m -> foldl' applyGrav m ms) ms

-- applies the velocity to the moons positions
applyVel :: Moons -> Moons
applyVel = map updatePos

-- updates a moons position based on its velocity
updatePos :: Moon -> Moon
updatePos m = m {pos_ = pos_ m L.^+^ vel_ m}

{- Part 1-}
-- Kinetic energy is the sum of abs value of velocity vector
kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (V3 x y z)) = abs x + abs y + abs z

-- Potential energy is the sum of abs value of position vector
potentialEnergy :: Moon -> Int
potentialEnergy (Moon (V3 x y z) _) = abs x + abs y + abs z

-- PE x KE
totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

{- Part 2-}
search :: Coord -> Moons -> Integer
search c m = go (timestep m) 0
  where
    go m' n
      | map (getCoord c . pos_) m == map (getCoord c . pos_) m' = n
      | otherwise = go (timestep m') (n + 1)

getCoord :: Coord -> Vector -> Int
getCoord X (V3 x _ _) = x
getCoord Y (V3 _ y _) = y
getCoord Z (V3 _ _ z) = z

parse :: IO [[Int]]
parse = do
  contents <-  TIO.readFile "input/dayTwelve.txt"
  return $ map (map (read . T.unpack) . T.splitOn (T.pack ",")) $ T.lines contents
