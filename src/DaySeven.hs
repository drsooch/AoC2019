module DaySeven (answersD7, parse, part1, part2) where

import           Data.List    (foldl', permutations)
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           IntCode

type AMachines = M.Map Integer AmplifierMachine

-- feedback loop keeps track of the machines currently in execution
data FeedbackLoop = FL { aMachines_ :: AMachines
                       , ptr_       :: [Integer]
                       , curr_      :: Integer
                       } deriving (Show)

answersD7 :: IO ()
answersD7 = do
  values <- parse
  print $ "Part One: " ++ show (part1 values)
  print $ "Part Two: " ++ show (part2 values)

-- machine into the next
runChain :: [Integer] -> [Integer] -> Integer
runChain mem = foldl' (runChain' mem) 0

-- helper for runChain, actually executes the machine
runChain' :: [Integer] -> Integer -> Integer -> Integer
runChain' mem prevOutput phase = last $ getMachineOutput mem [phase, prevOutput] []

-- 13848 -> answer
part1 :: [Integer] -> Integer
part1 mem = maximum $ map (runChain mem) perms
  where
    perms = permutations [0 .. 4]

-- 12932154 -> answer
part2 :: [Integer] -> Integer
part2 mem = maximum $ map (runFeedbackLoop mem) perms
  where
    perms = permutations [5 .. 9]

-- create the starter feedback loop
mkFL :: [Integer] -> [Integer] -> FeedbackLoop
mkFL mem phases = FL { aMachines_  = M.insert 0 machineA' aMachines
                     , ptr_        = ptr
                     , curr_       = 0
                     }
  where
    aMachines = M.fromList $ zip [0..] $ map (mkAM mem) phases
    -- the puzzle instructions require the first AM to have the phase setting plus a zero  in its input
    machineA = aMachines M.! 0
    machineA' = machineA {inputs_ = inputs_ machineA ++ [0]}
    ptr = cycle [0 .. (length' phases - 1)] :: [Integer]

-- Wrapper function to run and update an AM
runMachineMS :: AmplifierMachine -> AmplifierMachine
runMachineMS am = am { iMachine_ = im'
                     , imState_  = ms
                     , output_   = output
                     }
  where
    inputs = inputs_ am
    im = iMachine_ am
    (ms, im', output) = runMachineMS' im inputs

runMachineMS' :: IMachine -> [Integer] -> (MachineState, IMachine, [Integer])
runMachineMS' im inputs = do
  let (ms, im', output) = getMachineRWS' im inputs [Halt, LoadVal]
  if ms == Finished
    then (Finished, im', output)
  else if ms == NeedsInput && (length' inputs < (inpIx_ im + 1))
    then (NeedsInput, im', output)
  else (Runnable, im', output)

-- exported wrapper for running a FeedbackLoop
runFeedbackLoop :: [Integer] -> [Integer] -> Integer
runFeedbackLoop mem phases = runFeedbackLoop' $ mkFL mem phases

-- The guts of the work for FL is done here
-- Acquire the first available machine, run it and collect its output
-- feed its output into the next machine
-- and continue
runFeedbackLoop' :: FeedbackLoop -> Integer
runFeedbackLoop' feedback
  -- the "max" machine is the highest index and is the one that has the answer included
  | isFinished feedback = last $ output_ $ snd $ M.findMax (aMachines_ feedback)
  | otherwise           = runFeedbackLoop' feedback' {aMachines_ = newAMachines}
  where
    (nextAM, feedback')  = nextRunnable feedback
    (feederAM, feederFL) = nextMachine feedback'
    nextAMRan = runMachineMS nextAM
    updateState am_ = case (imState_ am_) of
                        Runnable   -> Runnable
                        NeedsInput -> Runnable
                        Finished   -> Finished
    feederAM' = feederAM { imState_ = updateState feederAM
                         , inputs_ = (inputs_ feederAM) ++ (output_ nextAMRan)
                         }
    newAMachines = M.insert (curr_ feedback') nextAMRan $ M.insert (curr_ feederFL) feederAM' (aMachines_ feedback')

-- get the next machine in the chain
nextMachine :: FeedbackLoop -> (AmplifierMachine, FeedbackLoop)
nextMachine fl = (am, fl {ptr_ = ptr', curr_ = curr' })
  where
    ([curr'], ptr') = splitAt 1 $ ptr_ fl
    am = (aMachines_ fl) M.! curr'

-- recursive function to find next runnable machine
nextRunnable :: FeedbackLoop -> (AmplifierMachine, FeedbackLoop)
nextRunnable fl = case (imState_ amNext) of
                    Runnable -> (amNext, fl')
                    _        -> nextRunnable fl'
  where
    (amNext, fl') = nextMachine fl

-- check if all AMachines are finished
isFinished :: FeedbackLoop -> Bool
isFinished fl = M.null $ M.filter (\a -> (imState_ a) /= Finished) (aMachines_ fl)

parse :: IO [Integer]
parse = do
  contents <- TIO.readFile "input/daySeven.txt"
  return $map (read . T.unpack) $ T.splitOn (T.pack ",") contents
