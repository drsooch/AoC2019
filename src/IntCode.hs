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
  , getMachineOutput'
  , runMachine
  , runChain
  , runFeedbackLoop
  )
where

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict       as M
import           Data.List                (foldl')

type Memory = M.Map Integer Integer
type AMachines = M.Map Integer AmplifierMachine
type Address = Integer

-- Aliasing the RWS for the IntCode Machine
type Program = RWS [Integer] [Integer] IMachine

-- IntMachine representation
data IMachine = IM { ip_    :: Address  -- Instruction Pointer Location
                   , mem_   :: Memory   -- Mapping memory addresses with their values
                   , inpIx_ :: Integer      -- Input index
                   , relbIx_ :: Integer      -- Relative Index
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
                           , output_   :: [Integer]
                           , inputs_   :: [Integer]
                           } deriving (Show)

-- feedback loop keeps track of the machines currently in execution
data FeedbackLoop = FL { aMachines_  :: AMachines
                       , ptr_        :: [Integer]
                       , curr_       :: Integer
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
        | IncRelB
        | Halt
        deriving (Show, Eq)

-- Parameter type for instructions
-- Pos = Position i.e. the value in a Source Address is an Address
-- Imm = Immediate i.e. the value in a Source Address is a value
-- Rel = Relative i.e. the value is in an address x values relative to source
data Param = Pos Address
           | Imm Integer
           | Rel Integer
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
getMachineOutput :: [Integer] -> [Integer] -> Integer
getMachineOutput mem inputs = last output
  where
    (_, _, output) = runRWS runMachine inputs (createIM mem)

getMachineOutput' :: [Integer] -> [Integer] -> [Integer]
getMachineOutput' mem inputs = output
  where
    (_, _, output) = runRWS runMachine inputs (createIM mem)

-- create an IntCode Machine from a list of memory values
createIM :: [Integer] -> IMachine
createIM xs = IM { ip_ = 0
                 , mem_ = mem
                 , inpIx_ = 0
                 , relbIx_ = 0
                 }
  where
    mem = M.fromAscList $ zip [0..] xs

-- Create an Amplifier Machine
createAM :: [Integer] -> Integer -> AmplifierMachine
createAM mem phase = AM { iMachine_ = (createIM mem)
                        , imState_  = Runnable
                        , output_   = []
                        , inputs_   = [phase]
                        }

-- create the starter feedback loop
createFeedbackLoop :: [Integer] -> [Integer] -> FeedbackLoop
createFeedbackLoop mem phases = FL { aMachines_  = M.insert 0 machineA' aMachines
                                   , ptr_        = ptr
                                   , curr_       = 0
                                   }
  where
    aMachines = M.fromList $ zip [0..] $ map (createAM mem) phases
    -- the puzzle instructions require the first AM to have the phase setting plus a zero  in its input
    machineA = aMachines M.! 0
    machineA' = machineA {inputs_ = inputs_ machineA ++ [0]}
    ptr = cycle [0 .. (length' phases - 1)] :: [Integer]

-- exported wrapper for running a FeedbackLoop
runFeedbackLoop :: [Integer] -> [Integer] -> Integer
runFeedbackLoop mem phases = runFeedbackLoop' $ createFeedbackLoop mem phases

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
  else if op == LoadVal && (length' inputs < (inpIx_ im + 1))
    then return NeedsInput
  else runMachineMS'

-- fold over a list of phase settings feeding the output from the previous
-- machine into the next
runChain :: [Integer] -> [Integer] -> Integer
runChain mem phases = foldl' (runChain' mem) 0 phases

-- helper for runChain, actually executes the machine
runChain' :: [Integer] -> Integer -> Integer -> Integer
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
    UnaryInst IncRelB _       -> execIncRel inst
    JumpInst _ _ _            -> execJump inst
    BinaryInst Equals _ _ _   -> execTest inst
    BinaryInst LessThan _ _ _ -> execTest inst
    BinaryInst _ _ _ _        -> execCompute inst
    HaltInst                  -> return Halt
    _                         -> error "Failed runStep"

-- Load a Value from the Reader Input
execLoad :: Instruction -> Program Op
execLoad inst = do
  inpVals <- ask
  im <- get
  let input = inpVals !! (fromIntegral $ inpIx_ im)  -- get the input value at the input index
  let mem' = insertP im (dest_ inst) input
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem', inpIx_ = (inpIx_ im) + 1} -- construct updated state
  return $ numToOp $ opCodeNum (mem' M.! ip')

-- Write a Value to Writer
execEmit :: Instruction -> Program Op
execEmit inst = do
  im <- get
  tell [lookupP im (dest_ inst)]
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip'}
  return $ (numToOp . opCodeNum) ((mem_ im) M.! ip')

-- Test a two Values and write the result
execTest :: Instruction -> Program Op
execTest inst = do
  im <- get
  let (val1, val2) = (lookupP im (src1_ inst), lookupP im (src2_ inst))
  let result = case opCode_ inst of
                 LessThan -> val1 < val2
                 Equals   -> val1 == val2
                 _        -> undefined
  let mem' = if result
             then insertP im (dest_ inst) 1
             else insertP im (dest_ inst) 0
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem' }
  return $ (numToOp . opCodeNum) (mem' M.! ip')

-- Test Jump condition
execJump :: Instruction -> Program Op
execJump inst = do
  im <- get
  let testVal = lookupP im (test_ inst)
  -- test based on Jump condition if evals to true we change ip to the 2nd parameter, else normal incIP
  let ip' = case (opCode_ inst) of
              JumpFalse -> if testVal == 0 then lookupP im (jumpTo_ inst) else incIP inst (ip_ im)
              JumpTrue  -> if testVal /= 0 then lookupP im (jumpTo_ inst) else incIP inst (ip_ im)
              _         -> undefined
  put im {ip_ = ip'}
  return $ (numToOp . opCodeNum) ((mem_ im) M.! ip')

-- execute an add or multiply
execCompute :: Instruction -> Program Op
execCompute inst = do
  im <- get
  let (val1, val2) = (lookupP im (src1_ inst), lookupP im (src2_ inst))
  let result = case opCode_ inst of
        Add  -> val1 + val2
        Mult -> val1 * val2
        x    -> error $ "Failed OpCode in execCompute" ++ (show x)
  let mem' = insertP im (dest_ inst) result
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem'}
  return $ (numToOp . opCodeNum) (mem' M.! ip')

execIncRel :: Instruction -> Program Op
execIncRel inst = do
  im <- get
  let relbIx' = (relbIx_ im) + lookupP im (dest_ inst)
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', relbIx_ = relbIx'}
  return $ (numToOp . opCodeNum) (mem_ im M.! ip')


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
                  9  -> uInst IncRelB
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

numToOp :: Integer -> Op
numToOp n = case n of
  1  -> Add
  2  -> Mult
  3  -> LoadVal
  4  -> EmitVal
  5  -> JumpTrue
  6  -> JumpFalse
  7  -> LessThan
  8  -> Equals
  9  -> IncRelB
  99 -> Halt
  _  -> error $ "failed numToOp"

-- increment the inst pointer based on operation
incIP :: Instruction -> Integer -> Integer
incIP (BinaryInst _ _ _ _) ip = ip + 4
incIP (JumpInst _ _ _)     ip = ip + 3
incIP (UnaryInst _ _)      ip = ip + 2
incIP _ ip                    = ip

-- lookup value based on Param Type
lookupP :: IMachine -> Param -> Integer
lookupP im (Pos addr) = (mem_ im) M.! addr
lookupP _ (Imm val)   = val
lookupP im (Rel x)    = (mem_ im) M.! (relbIx_ im + x)

-- insert value into Memory (fails if not correct param type)
insertP :: IMachine -> Param -> Integer -> Memory
insertP im (Pos addr) val = M.insert addr val (mem_ im)
insertP im (Rel x) val    = M.insert (relbIx_ im + x) val (mem_ im)
insertP _ (Imm _) _       = error "Inserting into immediate address"

-- translates Integer to Param
toParam :: Integer -> Integer -> Param
toParam addr 0 = Pos addr
toParam val  1 = Imm val
toParam val  2 = Rel val
toParam _ x    = error $ "Failed Parameter in toParam" ++ (show x)

-- take an instruction number and get a specific digit
paramMode :: Integer -> Integer -> Integer
paramMode inst digit = mod (div inst (10^digit)) 10

opCodeNum :: Integer -> Integer
opCodeNum n = mod n 100

-------------------------------------------------------------------------------------
-- Day Seven

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

length' :: [a] -> Integer
length' = fromIntegral . length
