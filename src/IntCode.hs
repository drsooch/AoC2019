module IntCode (IMachine, Op, Program, createIM, runMachine, runRWS) where

import           Control.Monad.RWS.Strict
import           Control.Monad.State
import qualified Data.IntMap.Strict       as M
import           Debug.Trace

type Memory = M.IntMap Int
type Address = Int

-- Aliasing the RWS for the IntCode Machine
type Program = RWS [Int] [Int] IMachine Op

-- IntMachine representation
data IMachine = IM { ip    :: Address  -- Instruction Pointer Location
                   , mem   :: Memory   -- Mapping memory addresses with their values
                   , inpIx :: Int      -- Input index
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
data Instruction = BinaryInst { opCode :: Op
                              , src1   :: Param
                              , src2   :: Param
                              , dest   :: Param
                              }
                 | UnaryInst  { opCode :: Op
                              , dest   :: Param
                              }
                 | JumpInst   { opCode :: Op
                              , test   :: Param
                              , jumpTo :: Param
                              }
                 | HaltInst
                 deriving (Show)

-- create an IntCode Machine from a list of memory values
createIM :: [Int] -> IMachine
createIM xs = IM {ip = 0, mem = mem', inpIx = 0}
  where
    mem' = M.fromAscList $ zip [0..] xs

-- runs a machine until a halt code is encountered
runMachine :: Program
runMachine = do
  op <- runStep
  if op == Halt
    then return Halt
    else runMachine

-- runs one instruction and updates the State
runStep :: Program
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
execLoad :: Instruction -> Program
execLoad inst = do
  inpVals <- ask
  im <- get
  let input = inpVals !! inpIx im  -- get the input value at the input index
  let mem' = insertP (mem im) (dest inst) input
  put im {ip = incIP inst (ip im), mem = mem', inpIx = (inpIx im) + 1} -- construct updated state
  return LoadVal

-- Write a Value to Writer
execEmit :: Instruction -> Program
execEmit inst = do
  im <- get
  tell [lookupP (mem im) (dest inst)]
  put im {ip = incIP inst (ip im)}
  return EmitVal

-- Test a two Values and write the result
execTest :: Instruction -> Program
execTest inst = do
  im <- get
  let (val1, val2) = (lookupP (mem im) (src1 inst), lookupP (mem im) (src2 inst))
  let result = case opCode inst of
                 LessThan -> val1 < val2
                 Equals   -> val1 == val2
  let mem' = if result
             then insertP (mem im) (dest inst) 1
             else insertP (mem im) (dest inst) 0
  put im {ip = incIP inst (ip im),  mem = mem' }
  return (opCode inst)

-- Test Jump condition
execJump :: Instruction -> Program
execJump inst = do
  im <- get
  let testVal = lookupP (mem im) (test inst)
  -- test based on Jump condition if evals to true we change ip to the 2nd parameter, else normal incIP
  let ip' = case (opCode inst) of
              JumpFalse -> if testVal == 0 then lookupP (mem im) (jumpTo inst) else incIP inst (ip im)
              JumpTrue  -> if testVal /= 0 then lookupP (mem im) (jumpTo inst) else incIP inst (ip im)
  put im {ip = ip'}
  return (opCode inst)

-- execute an add or multiply
execCompute :: Instruction -> Program
execCompute inst = do
  im <- get
  let (val1, val2) = (lookupP (mem im) (src1 inst), lookupP (mem im) (src2 inst))
  let result = case opCode inst of
        Add  -> val1 + val2
        Mult -> val1 * val2
        x    -> error $ "Failed OpCode in execCompute" ++ (show x)
  let mem' = insertP (mem im) (dest inst) result
  put im {ip = incIP inst (ip im), mem = mem'}
  return (opCode inst)

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
  where
    mem' = mem im
    [opNum, ip1, ip2, ip3] = map ((M.!) mem') [ip im .. (ip im) + 3] -- get each value in an address
    op = mod opNum 100  -- parse the OpCode out
    (p1, p2, p3) = (paramMode opNum 2, paramMode opNum 3, paramMode opNum 4) -- get each parameters mode type
    bInst i = BinaryInst i (toParam ip1 p1) (toParam ip2 p2) (toParam ip3 p3) -- using the derived values construct an instruction
    jInst i = JumpInst   i (toParam ip1 p1) (toParam ip2 p2)
    uInst i = UnaryInst  i (toParam ip1 p1)

-- increment the inst pointer based on operation
incIP :: Instruction -> Int -> Int
incIP (BinaryInst _ _ _ _) ip' = ip' + 4
incIP (JumpInst _ _ _)     ip' = ip' + 3
incIP (UnaryInst _ _) ip'      = ip' + 2
incIP _ ip'                    = ip'

-- lookup value based on Param Type
lookupP :: Memory -> Param -> Int
lookupP mem' (Pos addr) = mem' M.! addr
lookupP _ (Imm val)     = val

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

