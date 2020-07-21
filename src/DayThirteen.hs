module DayThirteen (answersD13, parse, part1, part2) where

import           Control.Applicative ((<|>))
import           Data.Either         (partitionEithers)
import           Data.List.Split     (chunksOf)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           IntCode             (IMachine, MachineState (..), Op (..),
                                      getMachineOutput, getMachineRWS', mkIM,
                                      resetInputIx)

type Coord = (Integer, Integer)
type Screen = Map Coord Tile

data Tile = Empty
          | Wall
          | Block
          | Paddle
          | Ball
          deriving (Show, Eq, Enum)

data Arcade = Arcade { iMachine_     :: IMachine
                     , machineState_ :: MachineState
                     , ballPos_      :: Coord
                     , paddlePos_    :: Coord
                     , screen_       :: Screen
                     , score_        :: Maybe Integer
                     } deriving (Show)

answersD13 :: IO ()
answersD13 = do
  values <- parse
  print $ part1 values
  print $ part2 (2 : tail values)

-- 376 -> answer
part1 :: [Integer] -> Int
part1 p =
  M.size                      -- Get the size of the map of blocks
  $ M.filter (== Block)       -- Filter out Tiles that aren't blocks
  $ snd
  $ mkScreen                  -- turn the output into a Screen
  $ getMachineOutput p [] []  -- run the machine

-- 18509 -> answer
part2 :: [Integer] -> Maybe Integer
part2 = score_ . runArcade . mkArcade

-- chunk machine output into 3-Tuple of Integers
-- turn the chunks into an either value (Left -> for score value, and Right -> for (Coord, Tile))
-- take all the right values and make that a new screen
-- if there is a left return it, else nothing
mkScreen :: [Integer] -> (Maybe Integer, Screen)
mkScreen inp = (score, M.fromList $ snd parts)
  where
    parts = partitionEithers $ map parseChunk $ chunk inp
    score = maybeHead $ fst parts

-- parse a single chunk of output
-- if it matches -1, 0 then the third item is the score
parseChunk :: (Integer, Integer, Integer) -> Either Integer (Coord, Tile)
parseChunk (-1, 0, score) = Left score
parseChunk (x, y, t)      = Right ((x, y), toEnum $ fromIntegral t)

-- take in an output list and chunk it into 3-Tuple
chunk :: [Integer] -> [(Integer, Integer, Integer)]
chunk = map transform . chunksOf 3
  where
    transform (x:y:z:_) = (x, y, z)

mkArcade :: [Integer] -> Arcade
mkArcade inp = Arcade { iMachine_ = mkIM inp
                      , machineState_ = Runnable
                      , ballPos_ = (0,0)
                      , paddlePos_ = (0,0)
                      , screen_ = M.empty
                      , score_ = Nothing
                      }

findTile :: Tile -> Screen -> Coord
findTile tile = head . M.keys . M.filter (== tile)

-- search the screen for the ball
findBall :: Screen -> Coord
findBall = findTile Ball

-- search the screen for the paddle
findPaddle :: Screen -> Coord
findPaddle = findTile Paddle

-- We want the paddle to follow the ball
-- if its LT (Enum = 0) we want to give input -1
-- EQ (Enum = 1) we want to give input 0
-- GT (Enum = 2) input of 1
movePaddle :: Arcade -> Integer
movePaddle a = fromIntegral $ 1 - fromEnum (compare (fst $ paddlePos_ a) (fst $ ballPos_ a))

-- while we have blocks left and the machine hasn't finished
-- no matter the machine state we can always give it an input and be safe
runArcade :: Arcade -> Arcade
runArcade a
  | blocksLeft a || machineState_ a /= Finished = runArcade a'
  | otherwise = a
  where
    input = movePaddle a
    a' = updateArcade a $ getMachineRWS' (iMachine_ a) [input] [LoadVal, Halt]

-- updates an arcade machine, takes whatever RWS we got
-- Must reset the input index for the IM
-- Output only provides cells that have been updated, must union with current screen
updateArcade :: Arcade -> (MachineState, IMachine, [Integer]) -> Arcade
updateArcade arcade (ms, im, out) = arcade { iMachine_ = im'
                                           , machineState_ = ms
                                           , ballPos_ = findBall screen'
                                           , paddlePos_ = findPaddle screen'
                                           , screen_ = screen'
                                           , score_ = score <|> score_ arcade
                                           }
  where
    im' = resetInputIx im
    (score, screen) = mkScreen out
    screen' = M.union screen (screen_ arcade)

-- utility for safe Prelude.head
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead x  = Just $ head x

-- are there blocks left?
blocksLeft :: Arcade -> Bool
blocksLeft = not . M.null . M.filter (== Block) . screen_

parse :: IO [Integer]
parse = do
  contents <- TIO.readFile "input/dayThirteen.txt"
  return $ map (read . T.unpack) $ T.splitOn (T.pack ",") contents
