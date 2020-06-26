{-# LANGUAGE PatternGuards #-}
module IntCode (
  -- Types
    IMachine
  , Op
  , Program
  -- Utility
  , createIM
  -- Run Machine
  , getMachineOutput
  , runMachine
  , runChain
  , runFeedbackLoop
  )
where

import           Control.Monad.RWS.Strict
import qualified Data.IntMap.Strict       as M
import           Data.List                (foldl')
import           Debug.Trace

type Memory = M.IntMap Int
type AMachines = M.IntMap AmplifierMachine
type Address = Int

-- Aliasing the RWS for the IntCode Machine
type Program = RWS [Int] [Int] IMachine

-- IntMachine representation
data IMachine = IM { ip_    :: Address  -- Instruction Pointer Location
                   , mem_   :: Memory   -- Mapping memory addresses with their values
                   , inpIx_ :: Int      -- Input index
                   } deriving (Show)

-- State of Machine
-- Runnable: Machine has input and is available to run
-- NeedsInput: Waiting for the previous machines input
-- Finished: Reached a halt instruction and is finished
data MachineState = Runnable
                  | NeedsInput
                  | Finished
                  deriving (Show, Eq)

-- AmplifierMachine wraps an IM with additional information
data AmplifierMachine = AM { iMachine_ :: IMachine
                           , imState_  :: MachineState
                           , output_   :: [Int]
                           , inputs_   :: [Int]
                           } deriving (Show)

-- feedback loop keeps track of the machines currently in execution
data FeedbackLoop = FL { aMachines_  :: AMachines
                       , ptr_        :: [Int]
                       , curr_       :: Int
                       } deriving (Show)

-- IntCode Machine Operation types
data Op = Add
        | Mult
        | LoadVal
        | EmitVal
        | JumpTrue
        | JumpFalse
        | LessThan
        | Equals
        | Halt
        deriving (Show, Eq)

-- Parameter type for instructions
-- Pos = Position i.e. the value in a Source Address is an Address
-- Imm = Immediate i.e. the value in a Source Address is a value
data Param = Pos Address
           | Imm Int
           deriving (Show)

-- Encodes an instruction into either a BinaryInst which is for Add/Mult
-- Unary Inst which is for Load/Emit
-- or Halt Inst
data Instruction = BinaryInst { opCode_ :: Op
                              , src1_   :: Param
                              , src2_   :: Param
                              , dest_   :: Param
                              }
                 | UnaryInst  { opCode_ :: Op
                              , dest_   :: Param
                              }
                 | JumpInst   { opCode_ :: Op
                              , test_   :: Param
                              , jumpTo_ :: Param
                              }
                 | HaltInst
                 deriving (Show)

-- Useful for days 2, 5, and 7 part 1:
-- runs a machine; given a memory layout and inputs
getMachineOutput :: [Int] -> [Int] -> Int
getMachineOutput mem inputs = last output
  where
    (_, _, output) = runRWS runMachine inputs (createIM mem)

-- create an IntCode Machine from a list of memory values
createIM :: [Int] -> IMachine
createIM xs = IM {ip_ = 0, mem_ = mem, inpIx_ = 0}
  where
    mem = M.fromAscList $ zip [0..] xs

-- Create an Amplifier Machine
createAM :: [Int] -> Int -> AmplifierMachine
createAM mem phase = AM { iMachine_ = (createIM mem)
                        , imState_  = Runnable
                        , output_   = []
                        , inputs_   = [phase]
                        }

-- create the starter feedback loop
createFeedbackLoop :: [Int] -> [Int] -> FeedbackLoop
createFeedbackLoop mem phases = FL { aMachines_  = M.insert 0 machineA' aMachines
                                   , ptr_        = ptr
                                   , curr_       = 0
                                   }
  where
    aMachines = M.fromList $ zip [0..] $ map (createAM mem) phases
    -- the puzzle instructions require the first AM to have the phase setting plus a zero  in its input
    machineA = aMachines M.! 0
    machineA' = machineA {inputs_ = inputs_ machineA ++ [0]}
    ptr = cycle [0 .. (length phases - 1)]

-- exported wrapper for running a FeedbackLoop
runFeedbackLoop :: [Int] -> [Int] -> Int
runFeedbackLoop mem phases = runFeedbackLoop' $ createFeedbackLoop mem phases

-- The guts of the work for FL is done here
-- Acquire the first available machine, run it and collect its output
-- feed its output into the next machine
-- and continue
runFeedbackLoop' :: FeedbackLoop -> Int
runFeedbackLoop' feedback
  -- the "max" machine is the highest index and is the one that has the answer included
  | isFinished feedback = last $ output_ $ snd $ M.findMax (aMachines_ feedback)
  | otherwise           = runFeedbackLoop' feedback' {aMachines_ = newAMachines}
  where
    (nextAM, feedback') = nextRunnable feedback
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

-- Wrapper function to run and update an AM
runMachineMS :: AmplifierMachine -> AmplifierMachine
runMachineMS am = am { iMachine_ = im'
                     , imState_  = ms
                     , output_   = output
                     }
  where
    inputs = inputs_ am
    im = iMachine_ am
    (ms, im', output) = runRWS runMachineMS' inputs im

-- Similar to runMachine but returns a MachineState instead
runMachineMS' :: Program MachineState
runMachineMS' = do
  op <- runStep
  inputs <- ask
  im <- get
  if op == Halt
    then return Finished
  else if op == LoadVal && (length inputs < (inpIx_ im + 1))
    then return NeedsInput
  else runMachineMS'

-- fold over a list of phase settings feeding the output from the previous
-- machine into the next
runChain :: [Int] -> [Int] -> Int
runChain mem phases = foldl' (runChain' mem) 0 phases

-- helper for runChain, actually executes the machine
runChain' :: [Int] -> Int -> Int -> Int
runChain' mem prevOutput phase = getMachineOutput mem [phase, prevOutput]

-- runs a machine until a halt code is encountered
-- the op returned by runStep indicates the next instructions type
runMachine :: Program Op
runMachine = do
  op <- runStep
  if op == Halt
    then return Halt
  else runMachine

-- runs one instruction and updates the State
runStep :: Program Op
runStep = do
  im <- get
  let inst = createInst im
  case inst of
    UnaryInst LoadVal _       -> execLoad inst
    UnaryInst EmitVal _       -> execEmit inst
    JumpInst _ _ _            -> execJump inst
    BinaryInst Equals _ _ _   -> execTest inst
    BinaryInst LessThan _ _ _ -> execTest inst
    BinaryInst _ _ _ _        -> execCompute inst
    HaltInst                  -> return Halt

-- Load a Value from the Reader Input
execLoad :: Instruction -> Program Op
execLoad inst = do
  inpVals <- ask
  im <- get
  let input = inpVals !! inpIx_ im  -- get the input value at the input index
  let mem' = insertP (mem_ im) (dest_ inst) input
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem', inpIx_ = (inpIx_ im) + 1} -- construct updated state
  return $ numToOp $ opCodeNum (mem' M.! ip')

-- Write a Value to Writer
execEmit :: Instruction -> Program Op
execEmit inst = do
  im <- get
  tell [lookupP (mem_ im) (dest_ inst)]
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip'}
  return $ (numToOp . opCodeNum) ((mem_ im) M.! ip')

-- Test a two Values and write the result
execTest :: Instruction -> Program Op
execTest inst = do
  im <- get
  let (val1, val2) = (lookupP (mem_ im) (src1_ inst), lookupP (mem_ im) (src2_ inst))
  let result = case opCode_ inst of
                 LessThan -> val1 < val2
                 Equals   -> val1 == val2
  let mem' = if result
             then insertP (mem_ im) (dest_ inst) 1
             else insertP (mem_ im) (dest_ inst) 0
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem' }
  return $ (numToOp . opCodeNum) (mem' M.! ip')

-- Test Jump condition
execJump :: Instruction -> Program Op
execJump inst = do
  im <- get
  let testVal = lookupP (mem_ im) (test_ inst)
  -- test based on Jump condition if evals to true we change ip to the 2nd parameter, else normal incIP
  let ip' = case (opCode_ inst) of
              JumpFalse -> if testVal == 0 then lookupP (mem_ im) (jumpTo_ inst) else incIP inst (ip_ im)
              JumpTrue  -> if testVal /= 0 then lookupP (mem_ im) (jumpTo_ inst) else incIP inst (ip_ im)
  put im {ip_ = ip'}
  return $ (numToOp . opCodeNum) ((mem_ im) M.! ip')

-- execute an add or multiply
execCompute :: Instruction -> Program Op
execCompute inst = do
  im <- get
  let (val1, val2) = (lookupP (mem_ im) (src1_ inst), lookupP (mem_ im) (src2_ inst))
  let result = case opCode_ inst of
        Add  -> val1 + val2
        Mult -> val1 * val2
        x    -> error $ "Failed OpCode in execCompute" ++ (show x)
  let mem' = insertP (mem_ im) (dest_ inst) result
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem'}
  return $ (numToOp . opCodeNum) (mem' M.! ip')

-- create an instruction
createInst :: IMachine -> Instruction
createInst im = case op of
                  1  -> bInst Add
                  2  -> bInst Mult
                  3  -> uInst LoadVal
                  4  -> uInst EmitVal
                  5  -> jInst JumpTrue
                  6  -> jInst JumpFalse
                  7  -> bInst LessThan
                  8  -> bInst Equals
                  99 -> HaltInst
                  _  -> error $ "invalid instruction code " ++ show im
  where
    mem = mem_ im
    [opNum, ip1, ip2, ip3] = map ((M.!) mem) [ip_ im .. (ip_ im) + 3] -- get each value in an address
    op = mod opNum 100  -- parse the OpCode out
    (p1, p2, p3) = (paramMode opNum 2, paramMode opNum 3, paramMode opNum 4) -- get each parameters mode type
    bInst i = BinaryInst i (toParam ip1 p1) (toParam ip2 p2) (toParam ip3 p3) -- using the derived values construct an instruction
    jInst i = JumpInst   i (toParam ip1 p1) (toParam ip2 p2)
    uInst i = UnaryInst  i (toParam ip1 p1)

numToOp :: Int -> Op
numToOp n = case n of
  1  -> Add
  2  -> Mult
  3  -> LoadVal
  4  -> EmitVal
  5  -> JumpTrue
  6  -> JumpFalse
  7  -> LessThan
  8  -> Equals
  99 -> Halt

-- increment the inst pointer based on operation
incIP :: Instruction -> Int -> Int
incIP (BinaryInst _ _ _ _) ip = ip + 4
incIP (JumpInst _ _ _)     ip = ip + 3
incIP (UnaryInst _ _) ip      = ip + 2
incIP _ ip                    = ip

-- lookup value based on Param Type
lookupP :: Memory -> Param -> Int
lookupP mem (Pos addr) = mem M.! addr
lookupP _ (Imm val)    = val

-- insert value into Memory (fails if not correct param type)
insertP :: Memory -> Param -> Int -> Memory
insertP mem' (Pos addr) val = M.insert addr val mem'
insertP _ (Imm _) _         = error "Inserting into immediate address"

-- translates Int to Param
toParam :: Int -> Int -> Param
toParam addr 0 = Pos addr
toParam val  1 = Imm val
toParam _ x    = error $ "Failed Parameter in toParam" ++ (show x)

-- take an instruction number and get a specific digit
paramMode :: Int -> Int -> Int
paramMode inst digit = mod (div inst (10^digit)) 10

opCodeNum :: Int -> Int
opCodeNum n = mod n 100

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
