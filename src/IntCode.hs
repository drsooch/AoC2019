{-# LANGUAGE PatternGuards #-}
module IntCode (
  -- Types
    IMachine (..)
  , Op (..)
  , Program
  , AmplifierMachine (..)
  -- Utility
  , Instruction (..)
  , MachineState (..)
  , mkIM
  , mkAM
  , nextInst
  , resetInputIx
  , length'
  -- Run Machine
  , getMachineOutput
  , getMachineOutput'
  , getMachineRWS
  , getMachineRWS'
  , runMachine
  , runStep
  )
where

import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict          as M

type Memory = M.Map Integer Integer
type Address = Integer

-- Aliasing the RWS for the IntCode Machine
type Program = RWS [Integer] [Integer] IMachine

-- IntMachine representation
data IMachine = IM { ip_     :: Address  -- Instruction Pointer Location
                   , mem_    :: Memory   -- Mapping memory addresses with their values
                   , inpIx_  :: Integer  -- Input index
                   , relbIx_ :: Integer  -- Relative Index
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


-- returns full output rather than just the last
getMachineOutput :: [Integer] -> [Integer] -> [Op] -> [Integer]
getMachineOutput mem inputs []  = snd $ evalRWS (runMachine [Halt]) inputs (mkIM mem)
getMachineOutput mem inputs ops = snd $ evalRWS (runMachine ops) inputs (mkIM mem)

-- alternate where you can pass in an IMachine instead of memory
getMachineOutput' :: IMachine -> [Integer] -> [Op] -> [Integer]
getMachineOutput' im inputs []  = snd $ evalRWS (runMachine [Halt]) inputs im
getMachineOutput' im inputs ops = snd $ evalRWS (runMachine ops) inputs im

-- same as getMachineOutput but returns value, state, and writer
getMachineRWS :: [Integer] -> [Integer] -> [Op] -> (MachineState, IMachine, [Integer])
getMachineRWS mem inputs []  = runRWS (runMachine [Halt]) inputs (mkIM mem)
getMachineRWS mem inputs ops = runRWS (runMachine ops) inputs (mkIM mem)

-- alternate where you can pass in an IMachine instead of memory
getMachineRWS' :: IMachine -> [Integer] -> [Op] -> (MachineState, IMachine, [Integer])
getMachineRWS' im inputs []  = runRWS (runMachine [Halt]) inputs im
getMachineRWS' im inputs ops = runRWS (runMachine ops) inputs im

-- create an IntCode Machine from a list of memory values
mkIM :: [Integer] -> IMachine
mkIM xs = IM { ip_ = 0
                 , mem_ = mem
                 , inpIx_ = 0
                 , relbIx_ = 0
                 }
  where
    mem = M.fromAscList $ zip [0..] xs

-- Create an Amplifier Machine
mkAM :: [Integer] -> Integer -> AmplifierMachine
mkAM mem phase = AM { iMachine_ = (mkIM mem)
                    , imState_  = Runnable
                    , output_   = []
                    , inputs_   = [phase]
                    }


-- runs a machine until a halt code is encountered
-- the op returned by runStep indicates the next instructions type
runMachine :: [Op] -> Program MachineState
runMachine pause = do
  op <- runStep
  if op `elem` pause
    then return $ case op of
                    Halt    -> Finished
                    LoadVal -> NeedsInput
                    _       -> Runnable
  else runMachine pause

-- runs one instruction and updates the State
runStep :: Program Op
runStep = do
  im <- get
  let inst = createInst im
  case inst of
    UnaryInst LoadVal _       -> execLoad inst
    UnaryInst EmitVal _       -> execEmit inst
    UnaryInst IncRelB _       -> execIncRel inst
    JumpInst {}               -> execJump inst
    BinaryInst Equals _ _ _   -> execTest inst
    BinaryInst LessThan _ _ _ -> execTest inst
    BinaryInst {}             -> execCompute inst
    HaltInst                  -> return Halt
    _                         -> error "Failed runStep"


-- Load a Value from the Reader Input
execLoad :: Instruction -> Program Op
execLoad inst = do
  inpVals <- ask
  im <- get
  let input = inpVals !! fromIntegral (inpIx_ im)  -- get the input value at the input index
  let mem' = insertP im (dest_ inst) input
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem', inpIx_ = inpIx_ im + 1} -- construct updated state
  return $ numToOp $ opCodeNum (safeLkup ip' mem')

-- Write a Value to Writer
execEmit :: Instruction -> Program Op
execEmit inst = do
  im <- get
  tell [lookupP im (dest_ inst)]
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip'}
  return $ (numToOp . opCodeNum) (safeLkup ip' (mem_ im))

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
  return $ (numToOp . opCodeNum) (safeLkup ip' mem')

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
  return $ (numToOp . opCodeNum) (safeLkup ip' (mem_ im))

-- execute an add or multiply
execCompute :: Instruction -> Program Op
execCompute inst = do
  im <- get
  let (val1, val2) = (lookupP im (src1_ inst), lookupP im (src2_ inst))
  let result = case opCode_ inst of
        Add  -> val1 + val2
        Mult -> val1 * val2
        x    -> error $ "Failed OpCode in execCompute" ++ show x
  let mem' = insertP im (dest_ inst) result
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', mem_ = mem'}
  return $ (numToOp . opCodeNum) (safeLkup ip' mem')

execIncRel :: Instruction -> Program Op
execIncRel inst = do
  im <- get
  let relbIx' = relbIx_ im + lookupP im (dest_ inst)
  let ip' = incIP inst (ip_ im)
  put im {ip_ = ip', relbIx_ = relbIx'}
  return $ (numToOp . opCodeNum) (safeLkup ip' (mem_ im))


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
    [opNum, ip1, ip2, ip3] = map (flip safeLkup mem) [ip_ im .. ip_ im + 3] -- get each value in an address
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
  x  -> error $ "failed numToOp" ++ show x

-- increment the inst pointer based on operation
incIP :: Instruction -> Integer -> Integer
incIP BinaryInst {} ip = ip + 4
incIP JumpInst   {} ip = ip + 3
incIP UnaryInst  {} ip = ip + 2
incIP _             ip = ip

-- lookup value based on Param Type
lookupP :: IMachine -> Param -> Integer
lookupP im (Pos addr) = safeLkup addr (mem_ im)
lookupP _ (Imm val)   = val
lookupP im (Rel x)    = safeLkup (relbIx_ im + x) (mem_ im)

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
toParam _ x    = error $ "Failed Parameter in toParam" ++ show x

-- take an instruction number and get a specific digit
paramMode :: Integer -> Integer -> Integer
paramMode inst digit = mod (div inst (10^digit)) 10

opCodeNum :: Integer -> Integer
opCodeNum n = mod n 100

nextInst :: IMachine -> Op
nextInst im = numToOp $ opCodeNum $ safeLkup (ip_ im) (mem_ im)

safeLkup :: Integer -> Memory -> Integer
safeLkup = M.findWithDefault 0

resetInputIx :: IMachine -> IMachine
resetInputIx im = im {inpIx_ = 0}

-- needed after switch from Int -> Integer
length' :: [a] -> Integer
length' = fromIntegral . length
